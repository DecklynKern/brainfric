use std::collections::HashMap;

use crate::args::*;
use crate::ir::*;

impl IRStatement {

    pub fn uses_reg_value(&self, check_reg: &Address) -> bool {

        match self {
            Self::MoveCell(_, from) | Self::SubCell(_, from) => *from == *check_reg,
            Self::WriteByte(reg) | Self::BeginWhile(reg, _) => *reg == *check_reg,
            Self::ReadByte(reg) => *reg == *check_reg, // add compiler flag
            _ => false
        }
    }

    pub fn references_reg(&self, check_reg: &Address) -> bool {
        match self {
            Self::MoveCell(to, from) | Self::SubCell(to, from) =>
                *from == *check_reg || to.contains(check_reg),
            Self::Alloc(reg, _, _) | Self::AddConst(reg, _) | Self::WriteByte(reg) |
            Self::ReadByte(reg) | Self::BeginWhile(reg, _) | Self::EndWhile(reg) |
            Self::Free(reg) =>
                *reg == *check_reg
        }
    }

    pub fn delete_reg(&mut self, check_reg: &Address) -> bool {

        match self {
            Self::Alloc(reg, _, _) | Self::AddConst(reg, _) | Self::Free(reg)
                => *reg == *check_reg,
            Self::MoveCell(to, from) | Self::SubCell(to, from) => {
                if *from == *check_reg {
                    panic!("attempted to delete used value")
                }
                else {

                    for idx in 0..to.len() {
                        if to[idx] == *check_reg {
                            to.remove(idx);
                            break;
                        }
                    }

                    false

                }
            }
            Self::WriteByte(reg) | Self::ReadByte(reg) | Self::BeginWhile(reg, _) => {
                if *reg == *check_reg {
                    panic!("attempted to delete used value")
                }

                false

            }
            Self::EndWhile(_) => false
        }
    }
}

#[derive(Debug)]
enum OptimizeAction {
    DeleteReg(Address),
    DeleteStatement,
    ReplaceStatement(Vec<IRStatement>),
    DeleteWhile(Address),
    DeleteIfEnds(Address),
    CombineAdd(u8, usize),
    CombinePrevMove(usize, Address, Vec<Address>),
    None
}

pub fn optimize(ir: &mut Vec<IRStatement>) -> u32 {

    let mut passes = 0;
    let mut known_value = HashMap::new();

    while optimize_pass(ir, &mut known_value) {

        passes += 1;
        known_value.clear();

        if !arg_show_optimization_steps() {
            continue;
        }

        println!("\n=== OPTIMIZER PASS #{passes} ===");
        for ir_statement in ir.iter() {
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
            IRStatement::Alloc(reg, _, can_delete) => {

                known_value.insert(*reg, Some(0));

                if *can_delete {

                    let mut delete = true;

                    for check_idx in (statement_idx + 1)..ir.len() {

                        let ir_statement = &ir[check_idx];

                        match ir_statement {
                            IRStatement::Free(reg2) => if reg == reg2 {
                                break;
                            }
                            IRStatement::BeginWhile(_, _) | IRStatement::EndWhile(_) => {
                                delete = false;
                                break;
                            }
                            _ => if ir_statement.uses_reg_value(reg) {
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

                        if let IRStatement::BeginWhile(_, _) = ir[check_idx] {
                            break;
                        }

                        if let IRStatement::EndWhile(_) = ir[check_idx] {
                            break;
                        }

                        if !ir[check_idx].references_reg(reg) {
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

                if let Some(num) = known_value[from]  {

                    let mut new_statements: Vec<_> = to.iter().map(
                        |reg| IRStatement::AddConst(*reg, num)
                    ).collect();

                    new_statements.push(IRStatement::AddConst(*from, (256 - num as u16) as u8));
                    action = OptimizeAction::ReplaceStatement(new_statements);

                }
                else  {

                    for reg in to {
                        known_value.insert(*reg, None);
                    }
                    
                    known_value.insert(*from, Some(0));

                    for check_idx in (0..statement_idx).rev() {

                        if let IRStatement::BeginWhile(_, _) = ir[check_idx] {
                            break;
                        }

                        if let IRStatement::EndWhile(_) = ir[check_idx] {
                            break;
                        }

                        if let IRStatement::MoveCell(to2, from2) = &ir[check_idx] {

                            if to2.contains(from) && !to.contains(from2) {
                                action = OptimizeAction::CombinePrevMove(check_idx, *from, to.clone());
                            }

                            if !to.is_empty() {
                                break;
                            }
                        }

                        if ir[check_idx].references_reg(from) {
                            break;
                        }
                    }
                }
            }
            IRStatement::SubCell(to, from) => {

                if let Some(num) = known_value[from]  {

                    let mut new_statements: Vec<_> = to.iter().map(
                        |reg| IRStatement::AddConst(*reg, (256 - num as u16) as u8)
                    ).collect();

                    new_statements.push(IRStatement::AddConst(*from, num));
                    action = OptimizeAction::ReplaceStatement(new_statements);

                }
            }
            IRStatement::WriteByte(_) => {},
            IRStatement::ReadByte(reg) => {
                known_value.insert(*reg, None);
            }
            IRStatement::BeginWhile(reg, run_once) => {

                if let Some(num) = known_value[reg] && num == 0 {
                    action = OptimizeAction::DeleteWhile(*reg);
                }
                else if let Some(num) = known_value[reg] && num != 0 && *run_once {
                    action = OptimizeAction::DeleteIfEnds(*reg);
                }
                else if !run_once {
                    for val in known_value.values_mut() {
                        *val = None;
                    }
                }
            }
            IRStatement::EndWhile(reg) => {
                
                // be more precise about what is being cleared
                for value in known_value.values_mut() {
                    let _ = std::mem::replace(value, None);
                }

                known_value.insert(*reg, Some(0));
            
            }
        }

        let mut did_action = true;

        match action {
            OptimizeAction::DeleteReg(reg) => {
                
                let mut check_idx = 0;

                while check_idx < ir.len() {

                    if ir[check_idx].delete_reg(&reg) {
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
            OptimizeAction::DeleteWhile(while_reg) => {

                while !ir.is_empty() {

                    match ir[statement_idx] {
                        IRStatement::EndWhile(reg) => if reg == while_reg {
                            break;
                        }
                        _ => {}
                    }

                    ir.remove(statement_idx);

                }

                ir.remove(statement_idx);

            }
            OptimizeAction::DeleteIfEnds(if_reg) => {

                for idx in ((statement_idx + 1)..ir.len()).rev() {

                    match ir[idx] {
                        IRStatement::EndWhile(reg) => if reg == if_reg {
                            ir.remove(idx);
                            break;
                        }
                        _ => {}
                    }
                }

                ir.remove(statement_idx);

            }
            OptimizeAction::CombineAdd(num, idx) => {

                let statement = &mut ir[idx];

                let mut insert = Vec::new();
                
                match statement {
                    IRStatement::AddConst(_, num2) => *num2 = num2.overflowing_add(num).0,
                    IRStatement::MoveCell(to,  _) => {
                        for reg in to {
                            insert.push(IRStatement::AddConst(*reg, num));
                        }
                    }
                    _ => todo!()
                }

                for item in insert {
                    ir.insert(idx, item);
                }

                ir.remove(statement_idx);

            }
            OptimizeAction::CombinePrevMove(idx, reg, replace_regs) => {

                if let IRStatement::MoveCell(to, _) = &mut ir[idx] {

                    let mut del_idx = None;

                    for i in 0..to.len() {
                        if to[i] == reg {
                            del_idx = Some(i);
                        }
                    }

                    if let Some(reg_idx) = del_idx {
                        to.remove(reg_idx);
                    }
                    else {
                        panic!()
                    }

                    to.extend(replace_regs);

                    ir.remove(statement_idx);

                }
                else {
                    panic!()
                }
            }
            OptimizeAction::ReplaceStatement(new_ir) => {

                ir.remove(statement_idx);

                for statement in new_ir {
                    ir.insert(statement_idx, statement);
                }
            }
            OptimizeAction::None => {
                did_action = false;
                statement_idx += 1
            }
        }

        if did_action {
            return true;
        }
    }

    false
    
}