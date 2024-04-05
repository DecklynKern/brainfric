use std::collections::HashMap;

use crate::ir::*;

pub fn lower(ir: Vec<IRStatement>) -> String {
    
    let mut lowerer = Lowerer {
        bf_code: String::new(),
        data_head: 0,
        stack_pointer: 0,
        identifier_table: HashMap::new()
    };

    lowerer.lower_statements(ir);
    std::mem::take(&mut lowerer.bf_code)

}

struct Lowerer {
    bf_code: String,
    data_head: usize,
    stack_pointer: usize,
    identifier_table: HashMap<Identifier, usize>
}

impl Lowerer {

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

    fn jump_to(&mut self, mem: Identifier) {
        
        let id_address = self.identifier_table[&mem];
        self.jump_diff(self.data_head, id_address);
        self.data_head = id_address;

    }

    fn interpret_named_code<const N: usize>(&mut self, code: &str, ids: [Identifier; N]) {
        for char in code.chars() {

            if char.is_whitespace() {
                continue;
            }

            match char {
                'a' => self.jump_to(ids[0]),
                'b' => self.jump_to(ids[1]),
                'c' => self.jump_to(ids[2]),
                'd' => self.jump_to(ids[3]),
                'e' => self.jump_to(ids[4]),
                'f' => self.jump_to(ids[5]),
                _ => self.bf_code.push(char)
            }
        }
    }

    fn lower_statements(&mut self, ir: Vec<IRStatement>) {
        
        for ir_statement in ir {
            
            match ir_statement {

                IRStatement::Alloc(allocation) => {
                    self.identifier_table.insert(allocation.get_identifier(), self.stack_pointer);
                    self.stack_pointer += 1;
                }
                IRStatement::Free(reg) => {

                    let memory = self.identifier_table[&reg];
                        
                    if self.stack_pointer - 1 == memory {
                        self.stack_pointer -= 1;
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
                IRStatement::WriteByteSequence(mem, len) => {

                    self.jump_to(mem);
                    
                    self.bf_code.push_str(&".>".repeat(len));
                    self.bf_code.push_str(&"<".repeat(len));

                }
                IRStatement::WriteString(mem) => {
                    self.jump_to(mem);
                    self.bf_code.push_str(">[.>]<[<]");
                }
                IRStatement::WriteByteAsNumber { id, temp_block } => {
                    self.interpret_named_code(
                        "b>++++++++++<a
                        [-b+>-[>+>>]>[+[-<+>]>+>>]<<<<<a]b>[-]>>>++++++++++<
                        [->-[>+>>]>[+[-<+>]>+>>]<<<<<]>[-]>>
                        [>++++++[-<++++++++>]<.<<+>+>[-]]<
                        [<[->-<]++++++[->++++++++<]>.[-]]<<++++++
                        [-<++++++++>]<.[-]<<[-a+b]",
                        [id, temp_block]
                    );
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