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
    identifier_table: HashMap<Identifier, (usize, usize)>
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

    fn jump_to(&mut self, ptr: Pointer) {
        
        let id_cell = self.identifier_table[&ptr.id].0;
        self.jump_diff(self.data_head, id_cell);
        
        self.jump_right(ptr.offset);

        self.data_head = id_cell + ptr.offset;

    }

    fn push_code(&mut self, code: &str) {
        
        let mut chars = code.chars();

        while let Some(char) = chars.next() {
            
            if char.is_whitespace() {
                continue;
            }

            self.bf_code.push(char);

        }
    }

    fn interpret_named_code<const N: usize>(&mut self, code: &str, ptrs: [Pointer; N]) {

        let mut chars = code.chars();

        while let Some(char) = chars.next() {

            if char.is_whitespace() {
                continue;
            }

            match char {
                'a' => self.jump_to(ptrs[0]),
                'b' => self.jump_to(ptrs[1]),
                'c' => self.jump_to(ptrs[2]),
                'd' => self.jump_to(ptrs[3]),
                'e' => self.jump_to(ptrs[4]),
                'f' => self.jump_to(ptrs[5]),
                _ => self.bf_code.push(char)
            }
        }
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
                    
                    self.identifier_table.insert(allocation.id, (self.stack_pointer, allocation.size));
                    self.stack_pointer += allocation.size;

                    println!("{:?}", self.identifier_table);

                }
                IRStatement::Free(reg) => {

                    let (cell_idx, size) = self.identifier_table[&reg];
                        
                    if self.stack_pointer == cell_idx + size {
                        self.stack_pointer -= size;
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
                IRStatement::WriteByteAsNumber {temp_block} => {

                    self.jump_to(temp_block.at(0));

                    self.push_code(
                        ">++++++++++<
                        [->-[>+>>]>[+[-<+>]>+>>]<<<<<]>[-]>>>++++++++++<
                        [->-[>+>>]>[+[-<+>]>+>>]<<<<<]>[-]>>
                        [>++++++[-<++++++++>]<.<<+>+>[-]]<
                        [<[->-<]++++++[->++++++++<]>.[-]]<
                        <++++++[-<++++++++>]<.[-]<<"
                    );
                }
                IRStatement::WriteShortAsNumber { ptr, temp_block } => {

                    // could be made more efficient maybe
                    // remove passed in ptr sometime

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
                        [ptr, temp_block.at(0)]
                    );
                }
                IRStatement::Compare(temp_block) => {

                    self.jump_to(temp_block.at(0));

                    self.push_code(
                        ">>>>+<<
                        [-<-[<]>>]
                        >[-<<<<+>>>]
                        >[-]<<[-]<[-]<"
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
                    
                    self.jump_to(temp_block.at(0));
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
                IRStatement::StackPop(ptr, size) => {

                    let cell_size = size + 1;

                    self.jump_to(ptr);
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