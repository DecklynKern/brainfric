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

    fn return_to_known_cell(&mut self) {

        let DataHeadState::Special(mem) = &self.data_head else {
            return;
        };

        for specifier in mem.offsets.iter().rev() {
            match specifier {
                Offset::Constant(offset) => {
                    let jump = self.get_jump_raw(-offset);
                    self.bf_code.push_str(&jump);
                }
            }
        }

        self.data_head = DataHeadState::KnownCell(self.identifier_table[&mem.identifier].address);

    }

    fn get_jump_raw(&self, jump_size: i32) -> Box<str> {
        if jump_size > 0 {
            ">"
        }
        else {
            "<"
        }
        .repeat(jump_size.unsigned_abs() as usize).into()
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

    fn jump_inner(&mut self, mem: MemoryAccess) {

        let DataHeadState::KnownCell(current_position) = self.data_head else {
            unreachable!();
        };
        
        let id_address = self.identifier_table[&mem.identifier].address;
        self.jump_diff(current_position, id_address);
        
        for specifier in mem.offsets.iter() {
            match specifier {
                Offset::Constant(offset) => self.bf_code.push_str(&self.get_jump_raw(*offset))
            }
        }

        self.data_head = DataHeadState::Special(mem);
    
    }

    fn jump_to(&mut self, mem: MemoryAccess) {

        // be more intellegent about jumps inside the same object
        if let DataHeadState::Special(data_head) = &self.data_head && mem == *data_head {
        }
        else {
            self.return_to_known_cell();
            self.jump_inner(mem);
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
                IRStatement::AddConst(mem, num) => {
                    
                    self.jump_to(mem);

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
                IRStatement::ReadByte(mem) => {
                    self.jump_to(mem);
                    self.bf_code.push(',');
                }
                IRStatement::WriteByte(mem) => {
                    self.jump_to(mem);
                    self.bf_code.push('.');
                }
                IRStatement::Loop(mem, block, _) => {
                    self.jump_to(mem.clone());
                    self.bf_code.push('[');
                    self.lower_statements(block.0);
                    self.jump_to(mem);
                    self.bf_code.push(']');
                }
            }
        }
    }
}