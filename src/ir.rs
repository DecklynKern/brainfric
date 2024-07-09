use std::collections::HashMap;
use std::mem::take;

use crate::args::arg_allow_delete_variables;
use crate::error::*;
use crate::err;
use crate::lex::Name;
use crate::elaborate::*;

pub type Identifier = usize;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Allocation {
    Variable(Identifier),
    Temporary(Identifier)
}

impl Allocation {

    pub fn can_delete(&self) -> bool {
        match self {
            Self::Variable(_) => arg_allow_delete_variables(),
            Self::Temporary(_) => true
        }
    }

    pub fn get_identifier(&self) -> Identifier {
        match self {
            Self::Variable(id) | Self::Temporary(id) => *id
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum ComplexOffset {
    Constant(i32)
}

#[derive(PartialEq, Eq, Default)]
pub struct IRBlock(pub Vec<IRStatement>);

impl IRBlock {

    pub fn new() -> Self {
        Self(Vec::new())
    }
}

impl std::fmt::Debug for IRBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {

        let err = self.0.iter().try_for_each(|statement| {
            write!(f, "\n  ");
            statement.fmt(f)
        });
        
        writeln!(f);
        err

    }
}

macro_rules! with_temp {

    ($self: ident, $temp: ident, $block: block) => {
        {
            let $temp = $self.allocate_temporary();
            $block
            $self.do_free($temp);
        }
    };

    ($self: ident, $temp: ident, $($other_temp: ident),*, $block: block) => {
        {
            let $temp = $self.allocate_temporary();
            with_temp!($self, $($other_temp),*, $block);
            $self.do_free($temp);
        }
    }
}

macro_rules! do_while {
    ($self: ident, $id: ident, $block: block) => {
        $self.do_begin_loop();
        $block;
        $self.do_end_loop($id, false);
    }
}

macro_rules! do_if {
    ($self: ident, $id: ident, $block: block) => {
        $self.do_begin_loop();
        $block;
        $self.do_clear_byte($id);
        $self.do_end_loop($id, true);
    }
}

macro_rules! do_if_bool {
    ($self: ident, $id: ident, $block: block) => {
        $self.do_begin_loop();
        $block;
        $self.do_sub_const($id, 1);
        $self.do_end_loop($id, true);
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum IRStatement {
    Alloc(Allocation),
    Free(Identifier),
    AddConst(Identifier, u8),
    MoveCell(Box<[(Identifier, bool)]>, Identifier),
    WriteByte(Identifier),
    WriteByteSequence(Identifier, usize),
    WriteByteAsNumber {id: Identifier, temp_block: Identifier}, // temp_block = 6 cells
    ReadByte(Identifier),
    Loop(Identifier, IRBlock, bool),
    Switch {temp_block: Identifier, arms: Vec<(u8, IRBlock)>, default: Option<IRBlock>}, // temp_block = 2 cells
}

pub fn generate_ir(elaborated_program: ElaboratedBlock) -> IRBlock {

    let mut ir_generator = IRGenerator {
        loop_stack: Vec::new(),
        ir: IRBlock::new(),
        next_identifier: 1,
        name_table: Vec::new()
    };

    ir_generator.generate_ir(elaborated_program)
    
}

struct IRGenerator {
    ir: IRBlock,
    loop_stack: Vec<IRBlock>,
    next_identifier: Identifier,
    name_table: Vec<Identifier>
}

impl IRGenerator {

    fn do_free(&mut self, id: Identifier) {
        self.free_block(id, 1);
    }

    fn free_block(&mut self, id: Identifier, size: u32) {
        for i in (id..(id + size as Identifier)).rev() {
            self.do_clear_byte(i);
            self.ir.0.push(IRStatement::Free(i));
        }
    }

    fn allocate_temporary(&mut self) -> Identifier {
        self.allocate_temporary_block(1)
    }

    fn allocate_temporary_block(&mut self, size: u32) -> Identifier {

        let id = self.next_identifier;

        for _ in 0..size {
            self.ir.0.push(IRStatement::Alloc(Allocation::Temporary(self.next_identifier)));
            self.next_identifier += 1;
        }

        id

    }

    fn resolve_accessor(&self, accessor: ElaboratedAccessor) -> Identifier {

        let mut true_id = self.name_table[accessor.name_id];

        for specifier in accessor.specifiers.iter() {
            match specifier {
                ElaboratedSpecifier::ConstOffset(offset) => {
                    true_id += offset;
                }
            }
        }

        true_id

    }

    fn do_move_raw<const N: usize>(&mut self, to: [(Identifier, bool); N], from: Identifier) {
        self.ir.0.push(IRStatement::MoveCell(
            to.iter()
                .map(|(mem, negate)| (*mem, *negate))
                .collect(),
            from)
        )
    }

    fn do_move<const N: usize>(&mut self, to: [Identifier; N], from: Identifier, negate: bool) {
        self.ir.0.push(IRStatement::MoveCell(
            to.into_iter().map(|mem| (mem, negate)).collect(),
            from
        ));
    }

    fn do_copy(&mut self, to: Identifier, from: Identifier, negate: bool) {
        with_temp!(self, temp, {
            self.do_move_raw([(temp, false), (to, negate)], from);
            self.do_move([from], temp, false);
        });
    }

    fn do_clear_byte(&mut self, id: Identifier) {
        self.ir.0.push(IRStatement::MoveCell(Box::default(), id));
    }

    fn do_add_const(&mut self, id: Identifier, val: u8) {
        if val != 0 {
            self.ir.0.push(IRStatement::AddConst(id, val));
        }
    }

    fn do_sub_const(&mut self, id: Identifier, val: u8) {
        self.do_add_const(id, val.wrapping_neg());
    }

    fn do_write_byte(&mut self, id: Identifier) {
        self.ir.0.push(IRStatement::WriteByte(id));
    }

    fn do_write_byte_as_number(&mut self, id: Identifier) {
        
        let temp_block = self.allocate_temporary_block(6);
        self.ir.0.push(IRStatement::WriteByteAsNumber { id, temp_block });

        self.free_block(temp_block, 6);
        
    }

    // assume temp is 0
    fn do_write_string_with_temp(&mut self, string: &str, temp: Identifier) {

        let mut val = 0u8;

        for chr in string.chars() {
            self.do_add_const(temp, (chr as u8).wrapping_sub(val));
            self.do_write_byte(temp);
            val = chr as u8;
        }
    }

    fn do_write_byte_sequence(&mut self, id: Identifier, length: usize) {
        for i in 0..length {
            self.do_write_byte(id + i);
        }
    }

    fn do_write_string(&mut self, string: &str) {
        with_temp!(self, temp, {
            self.do_write_string_with_temp(string, temp);
        });
    }

    fn do_copy_short(&mut self, to_low: Identifier, from_low: Identifier, negate: bool) {

        with_temp!(self, temp, {

            let to_high = to_low + 1;
            let from_high = from_low + 1;
    
            self.do_move([temp, to_high], from_high, negate);
            self.do_move([from_high], temp, negate);

            self.do_move([temp], from_low, negate);
            self.move_low_byte_to_short(to_low, temp, [(from_low, negate)]);

            if negate {
                self.do_sub_const(to_high, 1);
            }
        });
    }

    fn move_low_byte_to_short<const N: usize>(&mut self, short_id: Identifier, low_id: Identifier, copies: [(Identifier, bool); N]) {

        let short_high = short_id + 1;

        with_temp!(self, short_low_copy, {

            do_while!(self, low_id, {
            
                self.do_sub_const(low_id, 1);
                self.do_add_const(short_id, 1);

                for (copy_id, negate) in copies {
                    if negate {
                        self.do_sub_const(copy_id, 1);
                    }
                    else {
                        self.do_add_const(copy_id, 1);
                    }
                }
    
                self.do_move([short_low_copy], short_id, false);
                
                do_if!(self, short_low_copy, {
                    self.do_sub_const(short_high, 1);
                    self.do_move([short_id], short_low_copy, false);
                });
    
                self.do_add_const(short_high, 1);
    
            });
        });
    }

    fn do_add_const_short(&mut self, id: Identifier, val: u16) {

        self.do_add_const(id + 1, (val >> 8) as u8);

        with_temp!(self, add_lower_temp, {
            self.do_add_const(add_lower_temp, (val & 0xff) as u8);
            self.move_low_byte_to_short(id, add_lower_temp, []);
        });
    }

    fn do_sub_const_short(&mut self, id: Identifier, val: u16) {
        self.do_add_const_short(id, val.wrapping_neg());
    }

    fn do_clear_short(&mut self, id: Identifier) {
        self.ir.0.push(IRStatement::MoveCell(Box::default(), id));
        self.ir.0.push(IRStatement::MoveCell(Box::default(), id + 1));
    }

    fn push_ir_to_stack(&mut self) {
        self.loop_stack.push(take(&mut self.ir));
    }

    fn pop_ir_from_stack(&mut self) {
        self.ir = self.loop_stack.pop().unwrap();
    }

    fn do_begin_loop(&mut self) {
        self.push_ir_to_stack();
    }

    fn do_end_loop(&mut self, mem: Identifier, is_if: bool) {
        let ir = take(&mut self.ir);
        self.pop_ir_from_stack();
        self.ir.0.push(IRStatement::Loop(mem, ir, is_if));
    }

    fn evaluate_bool_expression_into(&mut self, expression: BoolExpression, into: Identifier) {

        match expression {
            BoolExpression::Access(accessor) => {
                let id = self.resolve_accessor(accessor);
                self.do_copy(into, id, false);
            }
            BoolExpression::Constant(value) => {
                self.do_add_const(into, value as u8);
            }
            BoolExpression::ConvertByte(expr) => {

                with_temp!(self, temp, {

                    self.evaluate_byte_expression_into(*expr, into, false);

                    do_if!(self, temp, {
                        self.do_add_const(into, 1);
                    });
                })
            }
            BoolExpression::Not(expr) => {

                with_temp!(self, temp, {

                    self.evaluate_bool_expression_into(*expr, temp);

                    self.do_add_const(into, 1);

                    do_if!(self, temp, {
                        self.do_sub_const(into, 1);
                    });
                });
            }
            BoolExpression::And(expr1, expr2) => {

                with_temp!(self, temp1, temp2, temp3, {

                    self.evaluate_bool_expression_into(*expr1, temp1);
                    self.evaluate_bool_expression_into(*expr2, temp2);

                    self.do_move([temp3], temp2, false);

                    do_if!(self, temp3, {
                        self.do_move([into], temp1, false);
                    });
                });
            }
            BoolExpression::Or(expr1, expr2) => {

                with_temp!(self, temp1, temp2, temp3, {

                    self.evaluate_bool_expression_into(*expr1, temp1);
                    self.evaluate_bool_expression_into(*expr2, temp2);
    
                    self.do_move([temp3], temp1, false);
                    self.do_move([temp3], temp2, false);

                    do_if!(self, temp3, {
                        self.do_add_const(into, 1);
                    });
                });
            }
            BoolExpression::ByteEquals(expr1, expr2) => {

                self.do_add_const(into, 1);

                with_temp!(self, temp, {

                    self.evaluate_byte_expression_into(*expr1, temp, false);
                    self.evaluate_byte_expression_into(*expr2, temp, true);

                    do_if!(self, temp, {
                        self.do_sub_const(into, 1);
                    });
                });
            }
            BoolExpression::ByteNotEquals(expr1, expr2) => {

                with_temp!(self, temp, {

                    self.evaluate_byte_expression_into(*expr1, temp, false);
                    self.evaluate_byte_expression_into(*expr2, temp, true);

                    do_if!(self, temp, {
                        self.do_add_const(into, 1);
                    });
                });
            }
            BoolExpression::ByteGreaterThan(expr1, expr2) => {

                with_temp!(self, temp1, temp2, temp3, {

                    self.evaluate_byte_expression_into(*expr1, temp1, false);
                    self.evaluate_byte_expression_into(*expr2, temp2, false);
    
                    self.do_sub_const(temp1, 1);

                    do_while!(self, temp1, {
    
                        self.do_add_const(into, 1);
                        self.do_sub_const(temp1, 1);
                        self.do_sub_const(temp2, 1);

                        do_if!(self, temp2, {
                            self.do_sub_const(into, 1);
                            self.do_move([temp3], temp2, false);
                        });
                        
                        self.do_move([temp2], temp3, false);

                    });
                });
            }
            BoolExpression::ByteLessThan(expr1, expr2) => {

                with_temp!(self, temp1, temp2, temp3, {

                    self.evaluate_byte_expression_into(*expr1, temp1, false);
                    self.evaluate_byte_expression_into(*expr2, temp2, false);

                    self.do_sub_const(temp2, 1);

                    do_while!(self, temp2, {

                        self.do_add_const(into, 1);
                        self.do_sub_const(temp1, 1);
                        self.do_sub_const(temp2, 1);

                        do_if!(self, temp1, {
                            self.do_sub_const(into, 1);
                            self.do_move([temp3], temp1, false);
                        });
                        
                        self.do_move([temp1], temp3, false);

                    });
                });
            }
            BoolExpression::ByteGreaterThanEqual(expr1, expr2) => {

                with_temp!(self, temp1, temp2, temp3, {

                    self.evaluate_byte_expression_into(*expr1, temp1, false);
                    self.evaluate_byte_expression_into(*expr2, temp2, false);

                    do_while!(self, temp1, {

                        self.do_add_const(into, 1);
                        self.do_sub_const(temp1, 1);
                        self.do_sub_const(temp2, 1);

                        do_if!(self, temp2, {
                            self.do_sub_const(into, 1);
                            self.do_move([temp3], temp2, false);
                        });
                        
                        self.do_move([temp2], temp3, false);

                    });
                });
            }
            BoolExpression::ByteLessThanEqual(expr1, expr2) => {

                with_temp!(self, temp1, temp2, temp3, {

                    self.evaluate_byte_expression_into(*expr1, temp1, false);
                    self.evaluate_byte_expression_into(*expr2, temp2, false);

                    do_while!(self, temp2, {

                        self.do_add_const(into, 1);
                        self.do_sub_const(temp1, 1);
                        self.do_sub_const(temp2, 1);

                        do_if!(self, temp1, {
                            self.do_sub_const(into, 1);
                            self.do_move([temp3], temp1, false);
                        });
                        
                        self.do_move([temp1], temp3, false);

                    });
                });
            }
            _ => todo!()
        }
    }

    fn evaluate_byte_expression_into(&mut self, expression: ByteExpression, into: Identifier, negate: bool) {

        match expression {
            ByteExpression::Access(accessor) => {
                let id = self.resolve_accessor(accessor);
                self.do_copy(into, id, false);
            }
            ByteExpression::Constant(value) => {
                if negate {
                    self.do_sub_const(into, value);
                }
                else {
                    self.do_add_const(into, value);
                }
            }
            ByteExpression::Add(expr1, expr2) => {
                self.evaluate_byte_expression_into(*expr1, into, negate);
                self.evaluate_byte_expression_into(*expr2, into, negate);
            }
            ByteExpression::Subtract(expr1, expr2) => {
                self.evaluate_byte_expression_into(*expr1, into, negate);
                self.evaluate_byte_expression_into(*expr2, into, !negate);
            }
            ByteExpression::Multiply(expr1, expr2) => {

                with_temp!(self, temp1, temp2, temp3, {

                    self.evaluate_byte_expression_into(*expr1, temp1, false);
                    self.evaluate_byte_expression_into(*expr2, temp2, false);

                    do_while!(self, temp2, {

                        self.do_move_raw([(into, negate), (temp3, false)], temp1);
                        self.do_move([temp1], temp3, false);
    
                        self.do_sub_const(temp2, 1);

                    });
                });
            }
            _ => todo!()
        };
    }

    fn evaluate_short_expression_into(&mut self, expression: ShortExpression, into: Identifier, negate: bool) {
        match expression {
            ShortExpression::Access(accessor) => {
                let id = self.resolve_accessor(accessor);
                self.do_copy_short(into, id, negate);
            }
            ShortExpression::Constant(value) => {
                if negate {
                    self.do_sub_const_short(into, value);
                }
                else {
                    self.do_add_const_short(into, value);
                }
            }
            ShortExpression::Add(expr1, expr2) => {
                self.evaluate_short_expression_into(*expr1, into, negate);
                self.evaluate_short_expression_into(*expr2, into, negate);
            }
            ShortExpression::Subtract(expr1, expr2) => {
                self.evaluate_short_expression_into(*expr1, into, negate);
                self.evaluate_short_expression_into(*expr2, into, !negate);
            }
            ShortExpression::Multiply(expr1, expr2) => {

                todo!("bruh moment")

            }
            _ => todo!()
        }
    }
    
    pub fn generate_ir(&mut self, mut block_statements: ElaboratedBlock) -> IRBlock {

        block_statements.reverse();
        
        while let Some(statement) = block_statements.pop() {
            
            match statement {

                ElaboratedStatement::Declaration(names, size) => {
            
                    for id in self.next_identifier..self.next_identifier + size {
                        self.ir.0.push(IRStatement::Alloc(Allocation::Variable(id)));
                    }
            
                    self.name_table.push(self.next_identifier);
                    self.next_identifier += size;

                }
                ElaboratedStatement::AssignBool(accessor, expression) => {

                    let id = self.resolve_accessor(accessor);

                    self.do_clear_byte(id);
                    self.evaluate_bool_expression_into(expression, id);

                }
                ElaboratedStatement::AssignByte(accessor, expression) => {

                    let id = self.resolve_accessor(accessor);

                    self.do_clear_byte(id);
                    self.evaluate_byte_expression_into(expression, id, false);

                }
                ElaboratedStatement::AssignShort(accessor, expression) => {

                    let id = self.resolve_accessor(accessor);

                    self.do_clear_short(id);
                    self.evaluate_short_expression_into(expression, id, false);

                }
                ElaboratedStatement::Increment(accessor, value) => {
                    let id = self.resolve_accessor(accessor);
                    self.do_add_const(id, value);
                }
                ElaboratedStatement::LeftShift(accessor, amount) => {
                    todo!()
                }
                ElaboratedStatement::RightShift(accessor, amount) => {
                    todo!()
                }
                ElaboratedStatement::Clear(accessor, size) => {
                    
                    let id = self.resolve_accessor(accessor);

                    for i in 0..size {
                        self.do_clear_byte(id + i);
                    }
                }
                ElaboratedStatement::WriteBool(expression) => {

                    with_temp!(self, temp1, temp2, {

                        self.evaluate_bool_expression_into(expression, temp1);

                        self.do_add_const(temp2, 1);

                        do_if_bool!(self, temp1, {
                            self.do_sub_const(temp2, 1);
                            self.do_write_string_with_temp("tru", temp2);
                        });

                        do_if_bool!(self, temp2, {
                            self.do_write_string_with_temp("fals", temp1);
                        });

                        // cute minor optimization
                        self.do_write_string_with_temp("e", temp1);

                    });
                }
                ElaboratedStatement::WriteByte(expression) => {

                    // todo: add optimizations for cases where copy is unnecessary

                    with_temp!(self, temp, {
                        self.evaluate_byte_expression_into(expression, temp, false);
                        self.do_write_byte(temp);
                    });
                }
                ElaboratedStatement::WriteShort(expression) => {
                    todo!()
                }
                ElaboratedStatement::WriteByteAsNum(expression) => {
                    with_temp!(self, temp, {
                        self.evaluate_byte_expression_into(expression, temp, false);
                        self.do_write_byte_as_number(temp);
                    });
                }
                ElaboratedStatement::WriteByteSequence(accessor, length) => {
                    let id = self.resolve_accessor(accessor);
                    self.do_write_byte_sequence(id, length);
                }
                ElaboratedStatement::WriteConstString(string) => {
                    self.do_write_string(&string);
                }
                ElaboratedStatement::ReadByte(accessor) => {
                    let id = self.resolve_accessor(accessor);
                    self.ir.0.push(IRStatement::ReadByte(id));
                }
                ElaboratedStatement::While(expression, loop_statements) => {

                    // optimization for special case
                    // also add for bool variable
                    if let BoolExpression::ConvertByte(box ByteExpression::Access(accessor)) = expression {

                        let id = self.resolve_accessor(accessor);

                        self.push_ir_to_stack();
                        let loop_ir = self.generate_ir(loop_statements);
                        self.ir.0.extend(loop_ir.0);
                        self.do_end_loop(id, false);

                        continue;

                    }
                    else {

                        with_temp!(self, temp, {

                            self.evaluate_bool_expression_into(expression.clone(), temp);
        
                            do_while!(self, temp, {
            
                                let loop_ir = self.generate_ir(loop_statements);
                                self.ir.0.extend(loop_ir.0);
            
                                self.do_clear_byte(temp);
                                self.evaluate_bool_expression_into(expression, temp);

                            });
                        });
                    }
                }
                ElaboratedStatement::If(expression, loop_statements, else_block) => {

                    with_temp!(self, temp, {

                        self.evaluate_bool_expression_into(expression, temp);

                        if let Some(block) = else_block {

                            with_temp!(self, temp2, {

                                self.do_add_const(temp2, 1);

                                do_if!(self, temp, {
        
                                    let if_ir = self.generate_ir(loop_statements);
                                    self.ir.0.extend(if_ir.0);
                
                                    self.do_sub_const(temp2, 1);
                                    
                                });

                                do_if!(self, temp2, {
                                    let else_ir = self.generate_ir(block);
                                    self.ir.0.extend(else_ir.0);
                                });
                            });
                        }
                        else {
                            do_if!(self, temp, {
                                let loop_ir = self.generate_ir(loop_statements);
                                self.ir.0.extend(loop_ir.0);
                            });
                        }
                    });
                }
                ElaboratedStatement::Switch(expression, mut arms, default_statements) => {

                    let temp_block = self.allocate_temporary_block(2);

                    self.evaluate_byte_expression_into(expression, temp_block + 1, false);

                    arms.sort_by(|(case1, _), (case2, _)| {
                        case1.cmp(case2)
                    });

                    self.push_ir_to_stack();

                    // add into_iter when rust 0.80 comes out
                    let arm_ir_blocks: Vec<(u8, IRBlock)> = Vec::from(arms).into_iter().map(|(case, block)| {

                        if case == 0 {
                            panic!("zero case not supported yet");
                        }

                        let ir = self.generate_ir(block);
                        (case, ir)
                        
                    }).collect();

                    let default = match default_statements {
                        Some(statements) => {
                            Some(self.generate_ir(statements))
                        }
                        None => None
                    };

                    self.pop_ir_from_stack();

                    self.ir.0.push(IRStatement::Switch {
                        temp_block,
                        arms: arm_ir_blocks,
                        default
                    });

                    self.free_block(temp_block, 2);
                    
                }
                _ => todo!("unimplemented statement type")
            }
        }

        take(&mut self.ir)
        
    }
}