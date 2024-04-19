use std::collections::HashMap;

use crate::ir::*;

pub fn lower(ir: IRBlock) -> String {
    
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

    fn jump_to(&mut self, id: Identifier) {
        
        let id_address = self.identifier_table[&id];
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

    fn lower_statements(&mut self, ir: IRBlock) {

        let original_head = self.data_head;
        
        for ir_statement in ir.0 {
            
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
                IRStatement::AddConst(id, num) => {
                    
                    self.jump_to(id);

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
                IRStatement::ReadByte(id) => {
                    self.jump_to(id);
                    self.bf_code.push(',');
                }
                IRStatement::WriteByte(id) => {
                    self.jump_to(id);
                    self.bf_code.push('.');
                }
                IRStatement::WriteByteSequence(id, len) => {

                    self.jump_to(id);
                    
                    self.bf_code.push_str(&".>".repeat(len));
                    self.bf_code.push_str(&"<".repeat(len));

                }
                IRStatement::WriteString(id) => {
                    self.jump_to(id);
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
                IRStatement::Loop(id, block, _) => {

                    self.jump_to(id);

                    self.bf_code.push('[');
                    self.lower_statements(block);
                    self.jump_to(id);
                    self.bf_code.push(']');

                }
                IRStatement::Switch {temp_block, arms, default} => {
                    
                    self.jump_to(temp_block);
                    self.bf_code.push_str("+>");

                    let mut prev_case = 0;

                    for (case, _) in arms.iter() {

                        self.bf_code.push_str(&"-".repeat((case - prev_case) as usize));
                        prev_case = *case;
                        
                        self.bf_code.push('[');

                    }

                    self.bf_code.push_str("<-");

                    if let Some(statements) = default {
                        self.lower_statements(statements);
                    }

                    self.bf_code.push_str(">[-]");

                    for (_, arm) in arms.into_iter().rev() {

                        self.bf_code.push_str("]<[-");
                        self.lower_statements(arm);
                        self.bf_code.push_str("]>");

                    }

                    self.bf_code.push('<');

                }
            }
        }

        self.jump_diff(self.data_head, original_head);
        self.data_head = original_head;

    }
}