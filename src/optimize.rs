use std::collections::HashMap;
use std::mem;

use crate::args::*;
use crate::ir::*;

pub fn optimize(ir: &mut IRBlock) -> [u32; 2] {

    let mut passes = [0, 0];

    let mut known_values = HashMap::new();

    while optimize_pass_stage1(&mut ir.0, &mut known_values) {

        passes[0] += 1;
        known_values.clear();

        if !arg_show_optimization_steps() {
            continue;
        }

        println!("\n=== OPTIMIZER STAGE 1 PASS {} ===", passes[0]);
        for ir_statement in ir.0.iter() {
            println!("{ir_statement:?}");
        }
    }

    while optimize_pass_stage2(&mut ir.0) {

        passes[1] += 1;

        if !arg_show_optimization_steps() {
            continue;
        }

        println!("\n=== OPTIMIZER STAGE 2 PASS {} ===", passes[1]);
        for ir_statement in ir.0.iter() {
            println!("{ir_statement:?}");
        }
    }

    passes

}

fn get_next_reference_idx(ir: &[IRStatement], id: Identifier, current_use_idx: usize) -> Option<usize> {
    ir.iter()
        .enumerate()
        .skip(current_use_idx + 1)
        .find_map(|(idx, statement)|
            statement.references_identifier(id).then_some(idx))
}

#[derive(Debug)]
enum OptimizeActionStage1 {
    DeleteStatement,
    ReplaceStatement(Box<[IRStatement]>),
    GuaranteeIf,
    CombineAdd(u8, usize),
    CombineMove(usize, Identifier, Box<[(Identifier, bool)]>),
    OptimizeLoop,
    None
}

fn find_optimization_stage1(ir: &[IRStatement], statement_idx: usize, known_values: &mut HashMap<usize, Option<u8>>) -> OptimizeActionStage1 {

    match &ir[statement_idx] {

        IRStatement::Alloc(allocation) => {
            known_values.insert(allocation.get_identifier(), Some(0));
        }
        IRStatement::Free(_) => {}
        IRStatement::AddConst(id, num) => {

            if *num == 0 {
                return OptimizeActionStage1::DeleteStatement;
            }

            if let Some(next_reference_idx) = get_next_reference_idx(ir, *id, statement_idx) {

                match &ir[next_reference_idx] {
                    IRStatement::AddConst(_, _) =>
                        return OptimizeActionStage1::CombineAdd(*num, next_reference_idx),
                    IRStatement::MoveCell(_, from) if *id == *from => 
                        return OptimizeActionStage1::CombineAdd(*num, next_reference_idx),
                    _ => {}
                }
            }

            if let Some(num2) = known_values[id] {
                known_values.insert(*id, Some(num.overflowing_add(num2).0));
            }
        }
        IRStatement::MoveCell(to, from) => {

            if let Some(num) = known_values[from] {

                // technically the second branch handles both
                // but this creates a lot less redundant passes
                return if num == 0 {
                    OptimizeActionStage1::DeleteStatement
                }
                else {

                    let negated = num.wrapping_neg();

                    OptimizeActionStage1::ReplaceStatement(
                        to.iter()
                            .map(|(id, negate)| 
                                IRStatement::AddConst(*id, if *negate {negated} else {num}))
                            .chain([IRStatement::AddConst(*from, negated)])
                            .collect()
                    )
                };
            }

            for (id, _) in to.iter() {
                known_values.insert(*id, None);
            }

            known_values.insert(*from, Some(0));

            for check_idx in (0..statement_idx).rev() {

                if let IRStatement::MoveCell(to2, from2) = &ir[check_idx] {

                    // add negation case eventually
                    if to2.contains(&(*from, false)) && !to.iter().any(|(id, _)| id == from2) {
                        return OptimizeActionStage1::CombineMove(check_idx, *from, to.clone());
                    }

                    if !to.is_empty() {
                        break;
                    }
                }

                if ir[check_idx].references_identifier(*from) {
                    break;
                }
            }
        }
        IRStatement::WriteByte(_) => {},
        IRStatement::ReadByte(id) => {
            known_values.insert(*id, None);
        }
        IRStatement::Loop(id, block, run_once) => {

            let known_value = known_values[id];

            // can maybe make loop optimization more powerful
            return if let Some(0) = known_value {
                OptimizeActionStage1::DeleteStatement
            }
            else if *run_once {

                let mut mutated_identifiers = block.get_mutated_identifiers();

                if mutated_identifiers.remove(id) && mutated_identifiers.is_empty() && !block.contains_io() {
                    OptimizeActionStage1::DeleteStatement
                }
                else {
                    match known_values[id] {
                        Some(_) => OptimizeActionStage1::GuaranteeIf,
                        None => OptimizeActionStage1::OptimizeLoop
                    }
                }
            }
            else {
                OptimizeActionStage1::OptimizeLoop
            };
        }
    }

    OptimizeActionStage1::None

}

fn optimize_pass_stage1(ir: &mut Vec<IRStatement>, known_values: &mut HashMap<usize, Option<u8>>) -> bool {

    let mut statement_idx = 0;

    while statement_idx < ir.len() {

        let mut did_action = true;

        match find_optimization_stage1(ir, statement_idx, known_values) {

            OptimizeActionStage1::DeleteStatement => {
                ir.remove(statement_idx);
            }
            OptimizeActionStage1::GuaranteeIf => {

                let IRStatement::Loop(_, mut block, true) = ir.remove(statement_idx) else {
                    panic!("guarantee branch error");
                }

                while let Some(statement) = block.0.pop() {
                    ir.insert(statement_idx, statement);
                }
            }
            OptimizeActionStage1::OptimizeLoop => {

                let IRStatement::Loop(id, block, run_once) = &mut ir[statement_idx] else {
                    panic!("loop optimization error")
                }

                let mutated_identifiers = block.get_mutated_identifiers();

                if !*run_once {
                    for address in &mutated_identifiers {
                        known_values.insert(*address, None);
                    }
                }
                    
                did_action = optimize_pass_stage1(&mut block.0, known_values);

                if !did_action {
                    did_action = optimize_pass_stage2(&mut block.0);
                }
                    
                for mutated_id in &mutated_identifiers {
                    known_values.insert(*mutated_id, None);
                }

                known_values.insert(*id, Some(0));
                    
            }
            OptimizeActionStage1::CombineAdd(num, idx) => {
                
                match &mut ir[idx] {
                    IRStatement::AddConst(_, num2) => {
                        *num2 = num2.overflowing_add(num).0;
                        vec![]
                    }
                    IRStatement::MoveCell(to,  _) => {

                        let negated = num.wrapping_neg();

                        to.iter()
                            .rev()
                            .map(|(id, negate)| IRStatement::AddConst(*id, if *negate {negated} else {num}))
                            .collect()
                    }
                    _ => panic!("combine add fail")
                }.into_iter().for_each(|item| ir.insert(idx + 1, item));

                ir.remove(statement_idx);

            }
            OptimizeActionStage1::CombineMove(idx, id, replace_ids) => {

                if let IRStatement::MoveCell(to, _) = &mut ir[idx] else {
                    panic!("combine move fail");
                }
                    
                let _ = mem::replace(
                    to,
                    to.iter()
                        .cloned()
                        .filter(|(addr, _)| *addr != id)
                        .chain(replace_ids.iter().cloned())
                        .collect::<Box<_>>()
                );
                    
                ir.remove(statement_idx);

            }
            OptimizeActionStage1::ReplaceStatement(new_statements) => {

                ir.remove(statement_idx);

                for statement in new_statements.into_vec() {
                    ir.insert(statement_idx, statement);
                }
            }
            OptimizeActionStage1::None => {
                did_action = false;
            }
        }
        
        statement_idx += 1;
        
        if did_action {
            return true;
        }
    }

    false
    
}

#[derive(Debug)]
enum OptimizeActionStage2 {
    DeleteIdentifier(Identifier),
    SwapPrevious,
    None
}

fn find_optimization_stage2(ir: &[IRStatement], statement_idx: usize) -> OptimizeActionStage2 {

    match ir[statement_idx] {

        IRStatement::Alloc(allocation) => {

            if !allocation.can_delete() {
                return OptimizeActionStage2::None;
            }

            let id = allocation.get_identifier();

            for ir_statement in ir.iter().skip(statement_idx + 1) {

                if let IRStatement::Free(id2) = ir_statement && id == *id2 {
                    return OptimizeActionStage2::DeleteIdentifier(id);
                }

                if ir_statement.reads_identifier_value(id) {
                    break;
                }
            }
        }
        IRStatement::AddConst(access1, _) => {
            
            if statement_idx != 0 && let IRStatement::AddConst(access2, _) = ir[statement_idx - 1] {

                // bubble sort by a different means
                if access1.get_identifier() < access2.get_identifier() {
                    return OptimizeActionStage2::SwapPrevious;
                }
            }
        }
        _ => {}
    }

    OptimizeActionStage2::None

}

fn optimize_pass_stage2(ir: &mut Vec<IRStatement>) -> bool {

    let mut statement_idx = 0;

    while statement_idx < ir.len() {

        let mut did_action = true;

        match find_optimization_stage2(ir, statement_idx) {
            OptimizeActionStage2::DeleteIdentifier(id) => 
                ir.retain_mut(|statement| !statement.delete_identifier(id)),
            OptimizeActionStage2::SwapPrevious =>
                ir.swap(statement_idx - 1, statement_idx),
            OptimizeActionStage2::None =>
                did_action = false,
        }
        
        statement_idx += 1;

        if did_action {
            return true;
        }
    }

    false

}
