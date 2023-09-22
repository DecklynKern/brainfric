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

        IRStatement::Alloc(mem, _) => {
            known_values.insert(mem.get_identifier(), Some(0));
        },
        IRStatement::Free(_) => {}
        IRStatement::AddConst(id, num) => {

            if *num == 0 {
                return OptimizeActionStage1::DeleteStatement;
            }
            else {

                for (check_idx, check_statement) in ir.iter().enumerate().skip(statement_idx + 1) {

                    if let IRStatement::While(_, _, _) = check_statement {

                        if check_statement.references_identifier(*id) {
                            break;
                        }
                    }

                    if !check_statement.references_identifier(*id) {
                        continue;
                    }

                    match check_statement {
                        IRStatement::AddConst(_, _) =>
                            return OptimizeActionStage1::CombineAdd(*num, check_idx),
                        IRStatement::MoveCell(_, from_id) if *id == *from_id =>
                            return OptimizeActionStage1::CombineAdd(*num, check_idx),
                        _ => {}
                    }

                    break;

                }

                if let Some(num2) = known_values[id] {
                    known_values.insert(*id, Some(num.overflowing_add(num2).0));
                }
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
            else  {

                for (id, _) in to.iter() {
                    known_values.insert(*id, None);
                }
                
                known_values.insert(*from, Some(0));

                for check_idx in (0..statement_idx).rev() {

                    if let IRStatement::MoveCell(to2, from2) = &ir[check_idx] {

                        if to2.contains(&(*from, true)) && !to.iter().any(|(id, _)| id == from2) {
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
        }
        IRStatement::WriteByte(_) => {},
        IRStatement::ReadByte(id) => {
            known_values.insert(*id, None);
        }
        IRStatement::While(id, _, run_once) => {

            return if *run_once {

                match known_values[id] {
                    Some(0) => OptimizeActionStage1::DeleteStatement,
                    Some(_) => OptimizeActionStage1::GuaranteeIf,
                    None => OptimizeActionStage1::OptimizeLoop
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

                if let IRStatement::While(_, mut block, true) = ir.remove(statement_idx) {

                    while let Some(statement) = block.0.pop() {
                        ir.insert(statement_idx + 1, statement);
                    }

                } else {
                    panic!("guarantee branch error");
                }

            }
            OptimizeActionStage1::OptimizeLoop => {

                if let IRStatement::While(id, block, _) = &mut ir[statement_idx] {

                    let used_addresses = block.get_used_identifiers();

                    for address in &used_addresses {
                        known_values.insert(*address, None);
                    }
                    
                    did_action = optimize_pass_stage1(&mut block.0, known_values);
                    
                    for address in &used_addresses {
                        known_values.insert(*address, None);
                    }

                    known_values.insert(*id, Some(0));

                }
                else {
                    panic!("loop optimization error")
                }
            }
            OptimizeActionStage1::CombineAdd(num, idx) => {
                
                for item in match &mut ir[idx] {
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
                    _ => panic!("combine add error")
                } {
                    ir.insert(idx + 1, item);
                }

                ir.remove(statement_idx);

            }
            OptimizeActionStage1::CombineMove(idx, id, replace_ids) => {

                if let IRStatement::MoveCell(to, _) = &mut ir[idx] {
                    
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
                else {
                    panic!()
                }
            }
            OptimizeActionStage1::ReplaceStatement(new_ir) => {

                ir.remove(statement_idx);

                for statement in new_ir.into_vec() {
                    ir.insert(statement_idx, statement);
                }
            }
            OptimizeActionStage1::None => {
                did_action = false;
            }
        }
        
        if did_action {
            return true;
        }
        
        statement_idx += 1;

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

        IRStatement::Alloc(mem, _) => {

            if mem.can_delete() {

                let id = mem.get_identifier();
                let mut delete = true;

                for ir_statement in ir.iter().skip(statement_idx + 1) {

                    match ir_statement {
                        IRStatement::Free(id2) => if id == *id2 {
                            break;
                        }
                        _ => if ir_statement.uses_identifier_value(id) {
                            delete = false;
                            break;
                        }
                    }
                }

                if delete {
                    return OptimizeActionStage2::DeleteIdentifier(id);
                }
            }
        }
        IRStatement::AddConst(id1, _) => {
            
            if statement_idx != 0 && let IRStatement::AddConst(id2, _) = ir[statement_idx - 1] {

                // bubble sort by a different means
                if id1 < id2 {
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

            OptimizeActionStage2::DeleteIdentifier(id) => {
                
                let mut check_idx = 0;

                while check_idx < ir.len() {

                    if ir[check_idx].delete_identifier(id) {
                        ir.remove(check_idx);
                    }
                    else {
                        check_idx += 1;
                    }
                }
            }
            OptimizeActionStage2::SwapPrevious => ir.swap(statement_idx - 1, statement_idx),
            OptimizeActionStage2::None => did_action = false,
        }
        
        if did_action {
            return true;
        }
        
        statement_idx += 1;

    }

    false

}
