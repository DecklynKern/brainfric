use std::collections::HashMap;

use crate::ir::*;
use crate::error::*;
use crate::err;

struct Memory {
    address: usize,
    size: usize
}

pub struct Lowerer {
    bf_code: String,
    data_head: usize,
    register_table: HashMap<Register, Memory>
}

impl Lowerer {

    pub fn new() -> Self {
        Self {
            bf_code: String::new(),
            data_head: 0,
            register_table: HashMap::new()
        }
    }

    pub fn jump_to(&mut self, reg: Register) {

        let memory = &self.register_table[&reg];
            
        let diff = memory.address as isize - self.data_head as isize;
        self.data_head = memory.address;

        if diff < 0 {
            for _ in 0..-diff {
                self.bf_code.push('<');
            }
        }
        else {
            for _ in 0..diff {
                self.bf_code.push('>');
            }
        }
    }

    pub fn lower(&mut self, ir: Vec<IRStatement>) -> String {
        
        let mut stack_pointer = 0;
        
        for ir_statement in ir {
            
            match ir_statement {
                IRStatement::Alloc(reg, size, _) => {

                    self.register_table.insert(reg, Memory {
                        address: stack_pointer,
                        size
                    });

                    stack_pointer += size;

                }
                IRStatement::Free(reg) => {

                    let memory = &self.register_table[&reg];
                        
                    if stack_pointer - memory.size == memory.address {
                        stack_pointer -= memory.size;
                    }
                }
                IRStatement::AddConst(reg, num) => {
                    
                    self.jump_to(reg);

                    if num > 127 {
                        for _ in 0..(255 - num) {
                            self.bf_code.push('-');
                        }
                    }
                    else {
                        for _ in 0..num {
                            self.bf_code.push('+');
                        }
                    }
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
                _ => {}
            }
        }

        let mut blank = String::new();
        std::mem::swap(&mut blank, &mut self.bf_code);
        blank
        
    }
}