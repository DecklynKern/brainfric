use std::collections::HashMap;
use std::mem;

use crate::args::*;
use crate::ir::*;

#[derive(Debug)]
enum OptimizeAction {
    DeleteReg(Address),
    DeleteStatement,
    ReplaceStatement(Box<[IRStatement]>),
    GuaranteeBranch,
    CombineAdd(u8, usize),
    CombinePrevMove(usize, Address, Box<[Address]>),
    DoLoopOptimization,
    None
}

pub fn optimize(ir: &mut IRBlock) -> u32 {

    let mut passes = 0;
    let mut known_value = HashMap::new();

    while optimize_pass(&mut ir.0, &mut known_value) {

        passes += 1;
        known_value.clear();

        if !arg_show_optimization_steps() {
            continue;
        }

        println!("\n=== OPTIMIZER PASS #{passes} ===");
        for ir_statement in ir.0.iter() {
            println!("{ir_statement:?}");
        }
    }

    passes

}

fn optimize_pass(ir: &mut Vec<IRStatement>, known_value: &mut HashMap<usize, Option<u8>>) -> bool {

    let mut statement_idx = 0;

    while statement_idx < ir.len() {

        let mut action = OptimizeAction::None;

        match &ir[statement_idx] {
            IRStatement::Alloc(reg, _, is_weak) => {

                known_value.insert(*reg, Some(0));

                if *is_weak {

                    let mut delete = true;

                    for check_idx in (statement_idx + 1)..ir.len() {

                        let ir_statement = &ir[check_idx];

                        match ir_statement {
                            IRStatement::Free(reg2) => if reg == reg2 {
                                break;
                            }
                            _ => if ir_statement.uses_address_value(*reg) {
                                delete = false;
                                break;
                            }
                        }
                    }

                    if delete {
                        action = OptimizeAction::DeleteReg(*reg);
                    }
                }
            }
            IRStatement::Free(_) => {}
            IRStatement::AddConst(reg, num) => {

                if *num == 0 {
                    action = OptimizeAction::DeleteStatement;
                }
                else {
                    
                    for check_idx in (statement_idx + 1)..ir.len() {

                        if let IRStatement::While(_, _, _) = &ir[check_idx] {

                            if ir[check_idx].references_address(*reg) {
                                break;
                            }
                        }

                        if !ir[check_idx].references_address(*reg) {
                            continue;
                        }

                        match ir[check_idx] {
                            IRStatement::AddConst(_, _) =>
                                action = OptimizeAction::CombineAdd(*num, check_idx),
                            IRStatement::MoveCell(_, from_reg) if *reg == from_reg =>
                                action = OptimizeAction::CombineAdd(*num, check_idx),
                            _ => {}
                        }

                        break;

                    }

                    if let OptimizeAction::None = action {
                        if let Some(num2) = known_value[reg] {
                            known_value.insert(*reg, Some(num.overflowing_add(num2).0));
                        }
                    }
                }
            }
            IRStatement::MoveCell(to, from) => {

                if let Some(num) = known_value[from] {

                    // technically the second branch handles both
                    // but this creates a lot less redundant passes
                    action = if num == 0 {
                        OptimizeAction::DeleteStatement
                    }
                    else {
    
                        OptimizeAction::ReplaceStatement(
                            to.into_iter()
                                .map(|reg| IRStatement::AddConst(*reg, num))
                                .chain([IRStatement::AddConst(*from, (256 - num as u16) as u8)])
                                .collect()
                        )
                    };
                }
                else  {

                    for reg in to.iter() {
                        known_value.insert(*reg, None);
                    }
                    
                    known_value.insert(*from, Some(0));

                    for check_idx in (0..statement_idx).rev() {

                        if let IRStatement::MoveCell(to2, from2) = &ir[check_idx] {

                            if to2.contains(from) && !to.contains(from2) {
                                action = OptimizeAction::CombinePrevMove(check_idx, *from, to.clone());
                            }

                            if !to.is_empty() {
                                break;
                            }
                        }

                        if ir[check_idx].references_address(*from) {
                            break;
                        }
                    }
                }
            }
            IRStatement::SubCell(to, from) => {

                if let Some(num) = known_value[from]  {

                    // technically the second branch handles both
                    // but this creates a lot less redundant passes
                    action = if num == 0 {
                        OptimizeAction::DeleteStatement
                    }
                    else {
    
                        OptimizeAction::ReplaceStatement(
                            to.into_iter()
                                .map(|reg| IRStatement::AddConst(*reg, (256 - num as u16) as u8))
                                .chain([IRStatement::AddConst(*from, num)])
                                .collect()
                        )
                    };
                }
            }
            IRStatement::WriteByte(_) => {},
            IRStatement::ReadByte(reg) => {
                known_value.insert(*reg, None);
            }
            IRStatement::While(reg, _, run_once) => {

                action = if *run_once {

                    match known_value[reg] {
                        Some(0) => OptimizeAction::DeleteStatement,
                        Some(_) => OptimizeAction::GuaranteeBranch,
                        None => OptimizeAction::DoLoopOptimization
                    }
                }
                else {
                    OptimizeAction::DoLoopOptimization
                }
            }
        }

        let mut did_action = true;

        match action {
            OptimizeAction::DeleteReg(reg) => {
                
                let mut check_idx = 0;

                while check_idx < ir.len() {

                    if ir[check_idx].delete_address(reg) {
                        ir.remove(check_idx);
                    }
                    else {
                        check_idx += 1;
                    }
                }
            }
            OptimizeAction::DeleteStatement => {
                ir.remove(statement_idx);
            }
            OptimizeAction::GuaranteeBranch => {

                if let IRStatement::While(_, mut block, true) = ir.remove(statement_idx) {

                    while let Some(statement) = block.0.pop() {
                        ir.insert(statement_idx + 1, statement);
                    }

                } else {
                    panic!("guarantee branch error");
                }

            }
            OptimizeAction::DoLoopOptimization => {

                if let IRStatement::While(reg, block, true)= &mut ir[statement_idx] {

                    let used_addresses = block.get_used_addresses();

                    for address in &used_addresses {
                        known_value.insert(*address, None);
                    }
                    
                    did_action = optimize_pass(&mut block.0, known_value);
                    
                    for address in &used_addresses {
                        known_value.insert(*address, None);
                    }

                    known_value.insert(*reg, Some(0));

                }
                else {
                    panic!("loop optimization error")
                }
            }
            OptimizeAction::CombineAdd(num, idx) => {

                let statement = &mut ir[idx];

                let mut insert = Vec::new();
                
                match statement {
                    IRStatement::AddConst(_, num2) => *num2 = num2.overflowing_add(num).0,
                    IRStatement::MoveCell(to,  _) => {
                        for reg in to.iter() {
                            insert.insert(0, IRStatement::AddConst(*reg, num));
                        }
                    }
                    _ => todo!()
                }

                for item in insert {
                    ir.insert(idx + 1, item);
                }

                ir.remove(statement_idx);

            }
            OptimizeAction::CombinePrevMove(idx, reg, replace_regs) => {

                if let IRStatement::MoveCell(to, _) = &mut ir[idx] {
                    
                    let _ = mem::replace(
                        to,
                        to.iter()
                            .cloned()
                            .filter(|addr| *addr != reg)
                            .chain(replace_regs.iter().cloned())
                            .collect::<Box<_>>()
                    );
                    
                    ir.remove(statement_idx);

                }
                else {
                    panic!()
                }
            }
            OptimizeAction::ReplaceStatement(new_ir) => {

                ir.remove(statement_idx);

                for statement in new_ir.into_vec() {
                    ir.insert(statement_idx, statement);
                }
            }
            OptimizeAction::None => {
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