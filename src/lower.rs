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
    identifier_table: HashMap<Identifier, Memory>
}

impl Lowerer {

    pub fn new() -> Self {
        Self {
            bf_code: String::new(),
            data_head: 0,
            stack_pointer: 0,
            identifier_table: HashMap::new()
        }
    }

    pub fn jump_to(&mut self, id: Identifier) {

        let memory_address = self.identifier_table[&id].address;

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

                IRStatement::Alloc(mem, size) => {

                    self.identifier_table.insert(mem.get_identifier(), Memory {
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
                IRStatement::AddConst(reg, num) => {
                    
                    self.jump_to(reg);

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

                    self.jump_to(from);
                    self.bf_code.push_str("[-");

                    for (reg, negate) in to.iter() {
                        self.jump_to(*reg);
                        self.bf_code.push(if *negate {'-'} else {'+'});
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
