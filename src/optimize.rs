use std::collections::HashMap;

use crate::ir::*;

enum OptimizeAction {
    DeleteReg(Register),
    DeleteStatement(usize),
    None
}

pub fn optimize(ir: &mut Vec<IRStatement>) -> bool {

    let mut statement_idx = 0;
    let mut known_zero = HashMap::new();

    let mut did_action = false;

    while statement_idx < ir.len() {

        let mut action = OptimizeAction::None;

        match &ir[statement_idx] {
            IRStatement::Alloc(reg, _, can_delete) => {

                known_zero.insert(*reg, true);

                if *can_delete {

                    let mut delete = true;

                    for check_idx in statement_idx..ir.len() {
                        if ir[check_idx].uses_reg_value(reg) {
                            delete = false;
                            break;
                        }
                    }

                    if delete {
                        action = OptimizeAction::DeleteReg(*reg);
                    }
                }
            }
            IRStatement::Free(_) => {}
            IRStatement::AddConst(reg, num) => {
                if known_zero[reg] && *num != 0 {
                    known_zero.insert(*reg, false);
                }
            }
            IRStatement::MoveCell(to, from) => {

                if known_zero[from] {
                    action = OptimizeAction::DeleteStatement(statement_idx);
                }
                else {

                    known_zero.insert(*from, true);
                
                    for reg in to {
                        known_zero.insert(*reg, false);
                    }
                }
            }
            IRStatement::WriteByte(_) => {},
            IRStatement::ReadByte(reg) => {
                known_zero.insert(*reg, false);
            }
        }

        match action {
            OptimizeAction::DeleteReg(reg) => {

                did_action = true;
                
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
            OptimizeAction::DeleteStatement(idx) => {
                did_action = true;
                ir.remove(idx);
            },
            OptimizeAction::None => statement_idx += 1
        }
    }

    did_action
    
}
