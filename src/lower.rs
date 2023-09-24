use std::collections::HashMap;

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

enum DataHeadState<'a> {
    KnownCell(usize),
    Special(&'a MemoryAccess)
}

struct Lowerer<'a> {
    bf_code: String,
    data_head: DataHeadState<'a>,
    stack_pointer: usize,
    identifier_table: HashMap<Identifier, Memory>
}

impl<'a> Lowerer<'a> {

    fn prepare_access(&mut self, access: &MemoryAccess) {
        match access {
            MemoryAccess::StackPush(_) => {
                self.jump_to(access);
                self.bf_code.push('+');
                self.return_to_known_cell();
            }
            _ => {}
        }
    }

    fn return_to_known_cell(&mut self) {
        match self.data_head {
            DataHeadState::KnownCell(_) => {}
            DataHeadState::Special(MemoryAccess::Identifier(id)) => {
                self.data_head = DataHeadState::KnownCell(self.identifier_table[id].address)
            }
            DataHeadState::Special(MemoryAccess::StackPush(access)) => {
                self.bf_code.push_str("[<]<<");
                self.data_head = DataHeadState::Special(access);
                self.return_to_known_cell();
            }
            DataHeadState::Special(MemoryAccess::StackPop(access)) => {
                
            }
        }
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
            .repeat(jump_size.abs() as usize)
        );
    }

    fn jump_inner(&mut self, access: &'a MemoryAccess) {

        match access {
            MemoryAccess::Identifier(id) => {
                if let DataHeadState::KnownCell(current_position) = self.data_head {
                    let id_address = self.identifier_table[&id].address;
                    self.jump_diff(current_position, id_address);
                    self.data_head = DataHeadState::KnownCell(id_address);
                }
                else {
                    unreachable!();
                }
            }
            MemoryAccess::StackPush(inner_access) => {
                self.jump_inner(inner_access);
                self.bf_code.push_str(">>[>]<");
            }
            _ => todo!()
        }
    }

    fn jump_to(&mut self, access: &'a MemoryAccess) {
        self.return_to_known_cell();
        self.jump_inner(access);
    }

    fn cleanup_access(&mut self, access: &MemoryAccess) {

        match access {
            MemoryAccess::StackPush(_) => {
                self.jump_to(access);
                self.bf_code.push('-');
                self.return_to_known_cell();
            }
            _ => {}
        }
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
                    
                    self.jump_to(&access);

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

                    self.jump_to(&from);
                    self.bf_code.push_str("[-");

                    for (reg, negate) in to.iter() {
                        self.jump_to(reg);
                        self.bf_code.push(if *negate {'-'} else {'+'});
                    }

                    self.jump_to(&from);
                    self.bf_code.push(']');

                }
                IRStatement::ReadByte(access) => {
                    self.jump_to(&access);
                    self.bf_code.push(',');
                }
                IRStatement::WriteByte(access) => {
                    self.jump_to(&access);
                    self.bf_code.push('.');
                }
                IRStatement::Loop(access, block, _) => {
                    self.jump_to(&access);
                    self.bf_code.push('[');
                    self.lower_statements(block.0);
                    self.jump_to(&access);
                    self.bf_code.push(']');
                }
            }
        }
    }
}