use std::collections::HashMap;

use crate::ir::*;

struct Memory {
    address: usize,
    size: usize
}

pub struct Lowerer {
    bf_code: String,
    data_head: usize,
    stack_pointer: usize,
    register_table: HashMap<Address, Memory>
}

impl Lowerer {

    pub fn new() -> Self {
        Self {
            bf_code: String::new(),
            data_head: 0,
            stack_pointer: 0,
            register_table: HashMap::new()
        }
    }

    pub fn jump_to(&mut self, reg: Address) {

        let memory_address = self.register_table[&reg].address;

        self.bf_code.push_str(&
            if memory_address > self.data_head {
                ">"
            }
            else {
                "<"
            }
        .repeat(memory_address.abs_diff(self.data_head)));
        
        self.data_head = memory_address;
        
    }

    fn lower_statements(&mut self, ir: Vec<IRStatement>) {
        
        for ir_statement in ir {
            
            match ir_statement {
                IRStatement::Alloc(reg, size, _) => {

                    self.register_table.insert(reg, Memory {
                        address: self.stack_pointer,
                        size
                    });

                    self.stack_pointer += size;

                }
                IRStatement::Free(reg) => {

                    let memory = &self.register_table[&reg];
                        
                    if self.stack_pointer - memory.size == memory.address {
                        self.stack_pointer -= memory.size;
                    }
                }
                IRStatement::AddConst(reg, num) => {
                    
                    self.jump_to(reg);

                    self.bf_code.push_str(&
                        if num > 127 {
                            "-".repeat(256 - num as usize)
                        }
                        else {
                            "+".repeat(num as usize)
                        }
                    );
                }
                IRStatement::MoveCell(to, from) => {

                    self.jump_to(from);
                    self.bf_code.push_str("[-");

                    for reg in to {
                        self.jump_to(reg);
                        self.bf_code.push('+');
                    }

                    self.jump_to(from);
                    self.bf_code.push(']');

                }
                IRStatement::SubCell(to, from) => {

                    self.jump_to(from);
                    self.bf_code.push_str("[-");

                    for reg in to {
                        self.jump_to(reg);
                        self.bf_code.push('-');
                    }

                    self.jump_to(from);
                    self.bf_code.push(']');

                }
                IRStatement::ReadByte(reg) => {
                    self.jump_to(reg);
                    self.bf_code.push(',');
                }
                IRStatement::WriteByte(reg) => {
                    self.jump_to(reg);
                    self.bf_code.push('.');
                }
                IRStatement::While(reg, block, _) => {
                    self.jump_to(reg);
                    self.bf_code.push('[');
                    self.lower_statements(block.0);
                    self.jump_to(reg);
                    self.bf_code.push(']');
                }
            }
        }
    }

    pub fn lower(&mut self, ir: Vec<IRStatement>) -> String {
        self.lower_statements(ir);
        std::mem::take(&mut self.bf_code)
    }
}
