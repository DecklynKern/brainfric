use std::collections::HashMap;

use crate::parse::*;
use crate::ir::*;

pub fn lower(ir: Vec<IRStatement>) -> String {
    
    let mut lowerer = Lowerer {
        bf_code: String::new(),
        data_head: DataHeadState::KnownCell(0),
        stack_pointer: 0,
        identifier_table: HashMap::new()
    };

    lowerer.lower_statements(ir);
    std::mem::take(&mut lowerer.bf_code)

}

struct Memory {
    address: usize,
    size: usize
}

enum DataHeadState {
    KnownCell(usize),
    Special(MemoryAccess)
}

struct Lowerer {
    bf_code: String,
    data_head: DataHeadState,
    stack_pointer: usize,
    identifier_table: HashMap<Identifier, Memory>
}

impl Lowerer {

    fn prepare_access(&mut self, access: &MemoryAccess) {

        for specifier in access.specifiers.iter() {
            match specifier {
                Specifier::StackTop => todo!()
            }
        }
        
        // match access {
        //     MemoryAccess::StackPush(_) => {
        //         self.jump_to(access);
        //         self.bf_code.push('+');
        //         self.return_to_known_cell();
        //     }
        //     _ => {}
        // }
    }

    fn cleanup_access(&mut self, access: &MemoryAccess) {

        for specifier in access.specifiers.iter() {
            match specifier {
                Specifier::StackTop => todo!()
            }
        }

        // match access {
        //     MemoryAccess::StackPush(_) => {
        //         self.jump_to(access);
        //         self.bf_code.push('-');
        //         self.return_to_known_cell();
        //     }
        //     _ => {}
        // }
    }


    fn return_to_known_cell(&mut self) {

        let DataHeadState::Special(mem) = &self.data_head else {
            return;
        };

        for specifier in mem.specifiers.iter().rev() {
            match specifier {
                Specifier::StackTop => todo!()
            }
        }

        self.data_head = DataHeadState::KnownCell(self.identifier_table[&mem.identifier].address);

    }

    fn jump_diff(&mut self, from: usize, to: usize) {
        
        self.bf_code.push_str(&
            if to > from {
                ">"
            }
            else {
                "<"
            }
            .repeat(to.abs_diff(from))
        );
    }

    fn jump_raw(&mut self, jump_size: i32) {
        
        self.bf_code.push_str(&
            if jump_size > 0 {
                ">"
            }
            else {
                "<"
            }
            .repeat(jump_size.unsigned_abs() as usize)
        );
    }

    fn jump_inner(&mut self, mem: MemoryAccess) {

        let DataHeadState::KnownCell(current_position) = self.data_head else {
            unreachable!();
        };
        
        let id_address = self.identifier_table[&mem.identifier].address;
        self.jump_diff(current_position, id_address);
        self.data_head = DataHeadState::KnownCell(id_address);

        for specifier in mem.specifiers.iter() {
            match specifier {
                Specifier::StackTop => todo!()
            }
        }
    }

    fn jump_to(&mut self, access: MemoryAccess) {
        self.return_to_known_cell();
        self.jump_inner(access);
    }

    fn lower_statements(&mut self, ir: Vec<IRStatement>) {
        
        for ir_statement in ir {
            
            match ir_statement {

                IRStatement::Alloc(allocation) => {

                    let size = allocation.get_size();

                    self.identifier_table.insert(allocation.get_identifier(), Memory {
                        address: self.stack_pointer,
                        size
                    });

                    self.stack_pointer += size;

                }
                IRStatement::Free(reg) => {

                    let memory = &self.identifier_table[&reg];
                        
                    if self.stack_pointer - memory.size == memory.address {
                        self.stack_pointer -= memory.size;
                    }
                }
                IRStatement::AddConst(access, num) => {
                    
                    self.jump_to(access);

                    self.bf_code.push_str(&
                        if num > 127 {
                            "-".repeat(num.wrapping_neg() as usize)
                        }
                        else {
                            "+".repeat(num as usize)
                        }
                    );
                }
                IRStatement::MoveCell(to, from) => {

                    self.jump_to(from.clone());
                    self.bf_code.push_str("[-");

                    for (reg, negate) in to.iter() {
                        self.jump_to(reg.clone());
                        self.bf_code.push(if *negate {'-'} else {'+'});
                    }

                    self.jump_to(from);
                    self.bf_code.push(']');

                }
                IRStatement::ReadByte(access) => {
                    self.jump_to(access);
                    self.bf_code.push(',');
                }
                IRStatement::WriteByte(access) => {
                    self.jump_to(access);
                    self.bf_code.push('.');
                }
                IRStatement::Loop(access, block, _) => {
                    self.jump_to(access.clone());
                    self.bf_code.push('[');
                    self.lower_statements(block.0);
                    self.jump_to(access);
                    self.bf_code.push(']');
                }
            }
        }
    }
}