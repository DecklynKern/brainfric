use std::fmt::Debug;
use std::mem::take;
use std::ops::Index;

use crate::args::arg_allow_delete_variables;
use crate::elaborate::*;

pub type Identifier = usize;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct TempBlock<const N: usize> {
    id: Identifier
}

impl<const N: usize> TempBlock<N> {
    pub fn at(&self, offset: usize) -> Pointer {
        Pointer {
            id: self.id,
            offset
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub struct Pointer {
    pub id: Identifier,
    pub offset: usize
}

impl Debug for Pointer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.offset > 0 {
            f.write_fmt(format_args!("{{{} [+{}]}}", self.id, self.offset))
        }
        else {
            f.write_fmt(format_args!("{{{}}}", self.id))
        }
    }
}

impl From<Identifier> for Pointer {
    fn from(id: Identifier) -> Self {
        Self {
            id,
            offset: 0
        }
    }
}

impl Pointer {
    pub fn with_offset(&self, offset: usize) -> Self {
        Self {
            id: self.id,
            offset: self.offset + offset
        }
    }
}



#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Allocation {
    pub id: Identifier,
    pub size: usize,
    pub is_temp: bool
}

impl Allocation {

    pub fn variable(id: Identifier, size: usize) -> Self {
        Self {
            id,
            size,
            is_temp: false
        }
    }

    pub fn temporary(id: Identifier, size: usize) -> Self {
        Self {
            id,
            size,
            is_temp: true
        }
    }

    pub fn can_delete(&self) -> bool {
        self.is_temp || arg_allow_delete_variables()
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
            write!(f, "\n  ")?;
            statement.fmt(f)
        });
        
        writeln!(f)?;
        err

    }
}

macro_rules! with_temp {

    ($self: ident, $temp: ident, $block: block) => {
        {
            let $temp = $self.allocate_temporary();
            $block
            $self.do_free($temp.id);
        }
    };

    ($self: ident, $temp: ident, $($other_temp: ident),*, $block: block) => {
        {
            let $temp = $self.allocate_temporary();
            with_temp!($self, $($other_temp),*, $block);
            $self.do_free($temp.id);
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
    ($self: ident, $id: expr, $block: block) => {
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
    AddConst(Pointer, u8),
    MoveCell(Box<[(Pointer, i8)]>, Pointer),
    WriteByte(Pointer),
    WriteByteSequence(Pointer, usize),
    WriteByteAsNumber {temp_block: TempBlock<6>},
    WriteShortAsNumber {ptr: Pointer, temp_block: TempBlock<9>},
    ReadByte(Pointer),
    Compare(TempBlock<5>), // less than or equal
    Loop(Pointer, IRBlock, bool),
    Switch {temp_block: TempBlock<2>, arms: Vec<(u8, IRBlock)>, default: Option<IRBlock>},
    StackPush(Pointer, usize),
    StackPop(Pointer, usize)
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
        self.ir.0.push(IRStatement::Free(id));
    }

    fn free_block<const N: usize>(&mut self, block: TempBlock<N>) {
    
        // for i in (block.id..(block.id + N)).rev() {
        //     self.do_clear_byte(block.at(i));
        // }

        self.ir.0.push(IRStatement::Free(block.id));
    
    }

    fn allocate_temporary(&mut self) -> Pointer {
        self.allocate_temporary_block::<1>().id.into()
    }

    fn allocate_temporary_block<const N: usize>(&mut self) -> TempBlock<N> {

        let id = self.next_identifier;
        self.next_identifier += 1;
        
        self.ir.0.push(IRStatement::Alloc(Allocation::temporary(id, N)));

        TempBlock::<N> {id}

    }

    fn resolve_accessor(&self, accessor: ElaboratedAccessor) -> Pointer {

        let id = self.name_table[accessor.name_id];
        let mut offset = 0;

        for specifier in accessor.specifiers.iter() {
            match specifier {
                ElaboratedSpecifier::ConstOffset(extra_offset) => {
                    offset += extra_offset;
                }
                _ => {}
            }
        }

        Pointer {
            id,
            offset
        }
    }

    fn do_move_raw<const N: usize>(&mut self, to: [(Pointer, bool); N], from: Pointer) {
        self.ir.0.push(IRStatement::MoveCell(
            to.into_iter()
                .map(|(mem, negate)| (
                    mem,
                    if negate {-1} else {1}
                ))
                .collect(),
            from)
        )
    }

    fn do_move<const N: usize>(&mut self, to: [Pointer; N], from: Pointer, negate: bool) {
        self.ir.0.push(IRStatement::MoveCell(
            to.into_iter().map(
                |mem| (
                    mem, 
                    if negate {-1} else {1}
                )).collect(),
            from
        ));
    }

    fn do_copy(&mut self, to: Pointer, from: Pointer, negate: bool) {

        let from_ptr = from;

        with_temp!(self, temp, {
            self.do_move_raw([(temp, false), (to, negate)], from_ptr);
            self.do_move([from_ptr], temp, false);
        });
    }

    fn do_clear_byte(&mut self, ptr: Pointer) {
        self.ir.0.push(IRStatement::MoveCell(Box::default(), ptr));
    }

    fn do_add_const(&mut self, ptr: Pointer, val: u8) {
        if val != 0 {
            self.ir.0.push(IRStatement::AddConst(ptr, val));
        }
    }

    fn do_sub_const(&mut self, ptr: Pointer, val: u8) {
        self.do_add_const(ptr, val.wrapping_neg());
    }

    fn do_write_byte(&mut self, ptr: Pointer) {
        self.ir.0.push(IRStatement::WriteByte(ptr));
    }

    fn do_write_short_as_number(&mut self, ptr: Pointer) {

        let temp_block = self.allocate_temporary_block();
        self.ir.0.push(IRStatement::WriteShortAsNumber {ptr, temp_block});

        self.free_block(temp_block);

    }

    // assume temp is 0
    fn do_write_string_with_temp(&mut self, string: &str, temp: Pointer) {

        let mut val = 0u8;

        for chr in string.chars() {
            self.do_add_const(temp, (chr as u8).wrapping_sub(val));
            self.do_write_byte(temp);
            val = chr as u8;
        }
    }

    fn do_write_byte_sequence(&mut self, ptr: Pointer, length: usize) {

        let write_ptr = ptr;

        for i in 0..length {
            self.do_write_byte(write_ptr.with_offset(i));
        }
    }

    fn do_write_string(&mut self, string: &str) {
        with_temp!(self, temp, {
            self.do_write_string_with_temp(string, temp);
        });
    }

    fn do_copy_short(&mut self, to_low: Pointer, from_low: Pointer, negate: bool) {

        with_temp!(self, temp, {

            let to_high = to_low.with_offset(1);
            let from_high = from_low.with_offset(1);
    
            self.do_move([temp, to_high], from_high, negate);
            self.do_move([from_high], temp, negate);

            self.do_move([temp], from_low, negate);
            self.move_low_byte_to_short(to_low, temp, [(from_low, negate)]);

            if negate {
                self.do_sub_const(to_high, 1);
            }
        });
    }

    fn move_low_byte_to_short<const N: usize>(&mut self, short_ptr: Pointer, low_ptr: Pointer, copies: [(Pointer, bool); N]) where [(); N + 2]: {

        let temp_block = self.allocate_temporary_block();

        with_temp!(self, temp, {

            let true_copies: [(Pointer, bool); N + 2] =
                [(temp_block.at(2), false), (short_ptr, false)]
                .iter()
                .chain(copies.iter())
                .cloned()
                .collect::<Vec<_>>()
                .try_into()
                .unwrap();

            self.do_move_raw(true_copies, low_ptr);
            self.do_move([low_ptr], temp, false);

        });
        
        self.do_copy(temp_block.at(1), short_ptr, true);
        self.do_sub_const(temp_block.at(1), 1);

        self.ir.0.push(IRStatement::Compare(temp_block));

        do_if!(self, temp_block.at(0), {
            self.do_add_const(short_ptr.with_offset(1), 1);
        });

        self.free_block(temp_block);

    }

    fn do_add_const_short(&mut self, ptr: Pointer, val: u16) {

        self.do_add_const(ptr.with_offset(1), (val >> 8) as u8);

        with_temp!(self, add_lower_temp, {
            self.do_add_const(add_lower_temp, (val & 0xff) as u8);
            self.move_low_byte_to_short(ptr, add_lower_temp, []);
        });
    }

    fn do_sub_const_short(&mut self, id: Pointer, val: u16) {
        self.do_add_const_short(id, val.wrapping_neg());
    }

    fn do_clear_short(&mut self, ptr: Pointer) {

        self.ir.0.push(IRStatement::MoveCell(Box::default(), ptr));
        self.ir.0.push(IRStatement::MoveCell(Box::default(), ptr.with_offset(1)));
    
    }

    fn do_clear_chunk(&mut self, ptr: Pointer, size: usize) {

        let start_ptr = ptr;

        for i in 0..size {
            self.do_clear_byte(start_ptr.with_offset(i));
        }
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

    fn do_end_loop(&mut self, ptr: Pointer, is_if: bool) {
        let ir = take(&mut self.ir);
        self.pop_ir_from_stack();
        self.ir.0.push(IRStatement::Loop(ptr, ir, is_if));
    }

    fn evaluate_bool_expression_into(&mut self, expression: BoolExpression, into: Pointer) {

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

                let temp_block = self.allocate_temporary_block();
                
                self.evaluate_byte_expression_into(*expr1, temp_block.at(1), false);
                self.evaluate_byte_expression_into(*expr2, temp_block.at(2), false);

                self.ir.0.push(IRStatement::Compare(temp_block));

                self.do_move([into], temp_block.at(0), true);
                self.do_add_const(into, 1);

                self.free_block(temp_block);

            }
            BoolExpression::ByteLessThan(expr1, expr2) => {

                let temp_block = self.allocate_temporary_block();
                
                self.evaluate_byte_expression_into(*expr2, temp_block.at(1), false);
                self.evaluate_byte_expression_into(*expr1, temp_block.at(2), false);

                self.ir.0.push(IRStatement::Compare(temp_block));

                self.do_move([into], temp_block.at(0), true);
                self.do_add_const(into, 1);

                self.free_block(temp_block);

            }
            BoolExpression::ByteGreaterThanEqual(expr1, expr2) => {

                let temp_block = self.allocate_temporary_block();
                
                self.evaluate_byte_expression_into(*expr2, temp_block.at(1), false);
                self.evaluate_byte_expression_into(*expr1, temp_block.at(2), false);

                self.ir.0.push(IRStatement::Compare(temp_block));

                self.do_move([into], temp_block.at(0), false);

                self.free_block(temp_block);

            }
            BoolExpression::ByteLessThanEqual(expr1, expr2) => {

                let temp_block = self.allocate_temporary_block();
                
                self.evaluate_byte_expression_into(*expr1, temp_block.at(1), false);
                self.evaluate_byte_expression_into(*expr2, temp_block.at(2), false);

                self.ir.0.push(IRStatement::Compare(temp_block));

                self.do_move([into], temp_block.at(0), false);

                self.free_block(temp_block);

            }
            _ => todo!()
        }
    }

    fn evaluate_byte_expression_into(&mut self, expression: ByteExpression, into: Pointer, negate: bool) {

        let into_ptr = into;

        match expression {
            ByteExpression::Access(accessor) => {
                let id = self.resolve_accessor(accessor);
                self.do_copy(into_ptr, id, false);
            }
            ByteExpression::ConvertBool(expr) => {
                assert!(!negate); // fix when not lazy
                self.evaluate_bool_expression_into(*expr, into);
            }
            ByteExpression::Constant(value) => {
                if negate {
                    self.do_sub_const(into_ptr, value);
                }
                else {
                    self.do_add_const(into_ptr, value);
                }
            }
            ByteExpression::Add(expr1, expr2) => {
                self.evaluate_byte_expression_into(*expr1, into_ptr, negate);
                self.evaluate_byte_expression_into(*expr2, into_ptr, negate);
            }
            ByteExpression::Subtract(expr1, expr2) => {
                self.evaluate_byte_expression_into(*expr1, into_ptr, negate);
                self.evaluate_byte_expression_into(*expr2, into_ptr, !negate);
            }
            ByteExpression::Multiply(expr1, expr2) => {

                with_temp!(self, temp1, temp2, temp3, {

                    self.evaluate_byte_expression_into(*expr1, temp1, false);
                    self.evaluate_byte_expression_into(*expr2, temp2, false);

                    do_while!(self, temp2, {

                        self.do_move_raw([(into_ptr, negate), (temp3, false)], temp1);
                        self.do_move([temp1], temp3, false);
    
                        self.do_sub_const(temp2, 1);

                    });
                });
            }
            ByteExpression::ConstMultiply(expr, coefficient) => {

                with_temp!(self, temp, {
                    self.evaluate_byte_expression_into(*expr, temp, negate);
                    self.ir.0.push(IRStatement::MoveCell(Box::new([(into, coefficient as i8)]), temp))
                });
            }
            _ => todo!()
        };
    }

    fn evaluate_short_expression_into(&mut self, expression: ShortExpression, into: Pointer, negate: bool) {
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

                ElaboratedStatement::Declaration(name_id, size) => {

                    // fix
                    assert!(name_id == self.name_table.len());
            
                    self.ir.0.push(IRStatement::Alloc(Allocation::variable(self.next_identifier, size)));
            
                    self.name_table.push(self.next_identifier);
                    self.next_identifier += 1;

                }
                ElaboratedStatement::AssignBool(accessor, expression) => {

                    let ptr = self.resolve_accessor(accessor);

                    self.do_clear_byte(ptr);
                    self.evaluate_bool_expression_into(expression, ptr);

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
                    self.do_clear_chunk(self.resolve_accessor(accessor), size);
                }
                ElaboratedStatement::WriteBool(expression) => {

                    with_temp!(self, temp1, temp2, {

                        self.evaluate_bool_expression_into(expression, temp1);

                        self.do_add_const(temp2, 1);

                        do_if_bool!(self, temp1, {
                            self.do_sub_const(temp2, 1);
                            self.do_write_string_with_temp("true", temp2);
                            self.do_clear_byte(temp2);
                        });

                        do_if_bool!(self, temp2, {
                            self.do_write_string_with_temp("false", temp1);
                        });
                    });
                }
                ElaboratedStatement::WriteByte(expression) => {

                    // don't copy if we don't have to
                    if let ByteExpression::Access(accessor) = expression {
                        let id = self.resolve_accessor(accessor);
                        self.do_write_byte(id);
                    }
                    else
                    {
                        with_temp!(self, temp, {
                            self.evaluate_byte_expression_into(expression, temp, false);
                            self.do_write_byte(temp);
                        });
                    }
                }
                ElaboratedStatement::WriteByteAsNum(expression) => {
        
                    let temp_block = self.allocate_temporary_block();

                    self.evaluate_byte_expression_into(expression, temp_block.at(0), false);

                    self.ir.0.push(IRStatement::WriteByteAsNumber {temp_block});
        
                    self.free_block(temp_block);

                }
                ElaboratedStatement::WriteShortAsNum(expression) => {

                    let temp = self.allocate_temporary_block::<2>();

                    self.evaluate_short_expression_into(expression, temp.at(0), false);
                    self.do_write_short_as_number(temp.at(0));

                    self.free_block(temp);

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

                    let temp_block = self.allocate_temporary_block();

                    self.evaluate_byte_expression_into(expression, temp_block.at(1), false);

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

                    self.free_block(temp_block);
                    
                }
                ElaboratedStatement::StackPush(id, size) => {
                    self.ir.0.push(IRStatement::StackPush(self.name_table[id].into(), size));
                }
                ElaboratedStatement::StackPop(id, size) => {
                    self.ir.0.push(IRStatement::StackPop(self.name_table[id].into(), size));
                }
                _ => todo!("unimplemented statement type")
            }
        }

        take(&mut self.ir)
        
    }
}