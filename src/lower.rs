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

    fn jump_left(&mut self, jump: usize) {
        self.bf_code.push_str(&"<".repeat(jump));
    }

    fn jump_right(&mut self, jump: usize) {
        self.bf_code.push_str(&">".repeat(jump));
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

    fn interpret_named_code_with_flag<const N: usize>(&mut self, code: &str, ids: [Identifier; N], flag: bool) {

        let mut chars = code.chars();

        while let Some(char) = chars.next() {

            if char.is_whitespace() || char == '}' {
                continue;
            }

            match char {
                'a' => self.jump_to(ids[0]),
                'b' => self.jump_to(ids[1]),
                'c' => self.jump_to(ids[2]),
                'd' => self.jump_to(ids[3]),
                'e' => self.jump_to(ids[4]),
                'f' => self.jump_to(ids[5]),
                '{' => {

                    if flag {
                        continue;
                    }

                    while let Some(block_char) = chars.next() {
                        if block_char == '}' {
                            break;
                        }
                    }
                }
                _ => self.bf_code.push(char)
            }
        }
    }

    fn interpret_named_code<const N: usize>(&mut self, code: &str, ids: [Identifier; N]) {
        self.interpret_named_code_with_flag(code, ids, true);
    }

    fn stack_traverse_in(&mut self, cell_size: usize) {
        self.bf_code.push('[');
        self.jump_right(cell_size);
        self.bf_code.push(']');
    }

    fn stack_traverse_out(&mut self, cell_size: usize) {
        self.bf_code.push('[');
        self.jump_left(cell_size);
        self.bf_code.push(']');
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
                IRStatement::WriteByteAsNumber { id, temp_block, destructive } => {
                    self.interpret_named_code_with_flag(
                        "b>++++++++++<a
                        [-b{+}>-[>+>>]>[+[-<+>]>+>>]<<<<<a]b>[-]>>>++++++++++<
                        [->-[>+>>]>[+[-<+>]>+>>]<<<<<]>[-]>>
                        [>++++++[-<++++++++>]<.<<+>+>[-]]<
                        [<[->-<]++++++[->++++++++<]>.[-]]<
                        <++++++[-<++++++++>]<.[-]<<{[-a+b]}",
                        [id, temp_block],
                        !destructive
                    );
                }
                IRStatement::WriteShortAsNumber { id, temp_block } => {

                    // could be made more efficient maybe
                    // also could implement non-destructive case

                    self.interpret_named_code(
                        "b>++++++++++<a
                        [-b>-[>+>>]>[+[-<+>]>+>>]<<<<<a]
                        b>[-]

                        >[-<<+>>]
                        >[-<<+>>]

                        ++++++++++<<<<
                        [->>>>-[>+>>]>[+[-<+>]>+>>]<<<<<<<<]
                        >>>>[-]>>>++++++++++<
                        [->-[>+>>]>[+[-<+>]>+>>]<<<<<]
                        
                        <[-<<++<+++++<++++++>>>>]
                        >>[-]>[-<<<<++<+++++<++++++>>>>>>]
                        >[-<<<<++<+++++<++++++>>>>>>]
                        
                        <<<++++++++++<<<<<
                        [->>>>>-[>+>>]>[+[-<+>]>+>>]<<<<<<<<<]
                        >>>>>[-]>[-<<<<<<+>>>>>>]>[-<<<<<<+>>>>>>]
                        
                        <<++++++++++<<<<
                        [->>>>-[>+>>]>[+[-<+>]>+>>]<<<<<<<<]
                        >>>>[-]>[-<<<<<+>>>>>]>[-<<<<<+>>>>>]
                        
                        <<++++++++++<<<
                        [->>>-[>+>>]>[+[-<+>]>+>>]<<<<<<<]
                        >>>[-]>[-<<<<+>>>>]>[-<<<<+>>>>]
                        
                        <<++++++++++<<
                        [->>-[>+>>]>[+[-<+>]>+>>]<<<<<<]
                        >>[-]>>[-<<<+>>>]

                        <<<[>++++++[-<++++++++>]<.>+>+<<[-]]
                        >>[<[->-<]++++++[->++++++++<]>.<<<+<+>>>>[-]]
                        <<<<[>[-<->]++++++[-<++++++++>]<.>+<<+>[-]]
                        <[>>[-<<->>]<++++++[-<++++++++>]<.[-]]
                        ++++++[-<++++++++>]<.[-]",
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
                IRStatement::StackPush(id, size) => {

                    let cell_size = size + 1;

                    self.jump_to(id);

                    for offset in 0..size {

                        self.bf_code.push_str(">[-");
                        self.jump_right(size - offset);

                        self.stack_traverse_in(cell_size);

                        self.jump_right(offset + 1);
                        self.bf_code.push('+');
                        self.jump_left(cell_size + offset + 1);

                        self.stack_traverse_out(cell_size);

                        self.jump_right(offset + 1);
                        self.bf_code.push(']');

                    }

                    self.bf_code.push('>');

                    self.stack_traverse_in(cell_size);
                    self.bf_code.push('+');
                    self.stack_traverse_out(cell_size);

                }
                IRStatement::StackPop(id, size) => {

                    let cell_size = size + 1;

                    self.jump_to(id);
                    self.jump_right(cell_size);

                    self.stack_traverse_in(cell_size);

                    for offset in 0..size {

                        self.jump_left(size - offset);
                        self.bf_code.push_str("[-");
                        self.jump_left(offset + 1);
                        self.stack_traverse_out(cell_size);

                        self.jump_right(offset + 1);
                        self.bf_code.push('+');
                        self.jump_right(cell_size - offset + 1);

                        self.stack_traverse_in(cell_size);

                        self.jump_left(size - offset);
                        self.bf_code.push(']');

                    }

                    self.jump_left(size);
                    self.bf_code.push('-');
                    self.jump_left(cell_size);

                    self.stack_traverse_out(cell_size);

                }
            }
        }

        self.jump_diff(self.data_head, original_head);
        self.data_head = original_head;

    }
}