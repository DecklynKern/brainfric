use std::collections::HashMap;
use std::mem::take;

use crate::args::arg_allow_delete_variables;
use crate::error::*;
use crate::err;
use crate::lex::Name;
use crate::parse::*;

use super::types::*;

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

impl Expression {
    fn contains_name(&self, name: &str) -> bool {
        match self {
            Self::Access(access) => access.name.as_ref() == name,
            Self::Equals(expr1, expr2) |
            Self::NotEquals(expr1, expr2) |
            Self::LessThan(expr1, expr2) | 
            Self::GreaterThan(expr1, expr2) |
            Self::And(expr1, expr2) |
            Self::Or(expr1, expr2) | 
            Self::Add(expr1, expr2) |
            Self::Subtract(expr1, expr2) |
            Self::Multiply(expr1, expr2) |
            Self::Divide(expr1, expr2) =>
                expr1.contains_name(name) || expr2.contains_name(name),
            Self::Not(expr) | Self::AsBool(expr) | Self::AsNum(expr) =>
                expr.contains_name(name),
            _ => false
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
        $self.do_clear($id);
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
    WriteString(Identifier),
    WriteByteAsNumber {id: Identifier, temp_block: Identifier}, // temp_block = 6 cells
    ReadByte(Identifier),
    Loop(Identifier, IRBlock, bool),
    Switch {temp_block: Identifier, arms: Vec<(u8, IRBlock)>, default: Option<IRBlock>}, // temp_block = 2 cells
}

pub fn generate_ir(parsed_program: ParsedProgram) -> Result<IRBlock, BrainFricError> {

    let user_definitions = UserDefinitions::new(parsed_program.definitions)?;

    let mut ir_generator = IRGenerator {
        loop_stack: Vec::new(),
        ir: IRBlock::new(),
        current_line_num: 0,
        next_identifier: 1,
        name_table: HashMap::new(),
        user_definitions
    };

    ir_generator.generate_ir(parsed_program.code)
    
}

struct IRGenerator {
    ir: IRBlock,
    loop_stack: Vec<IRBlock>,
    current_line_num: usize,
    next_identifier: Identifier,
    name_table: HashMap<Name, (Identifier, DataType)>,
    user_definitions: UserDefinitions
}

impl IRGenerator {

    fn do_free(&mut self, id: Identifier) {
        self.free_block(id, 1);
    }

    fn free_block(&mut self, id: Identifier, size: u32) {
        for i in (id..(id + size as Identifier)).rev() {
            self.do_clear(i);
            self.ir.0.push(IRStatement::Free(i));
        }
    }

    fn try_free_access(&mut self, mem: Identifier, is_temp: bool) {
        if is_temp {
            let id = mem;
            self.do_clear(id);
            self.ir.0.push(IRStatement::Free(id));
        }
    }

    fn assert_data_type(&self, expected_type: &DataType, got_type: &DataType) -> Result<(), BrainFricError> {

        if *expected_type != *got_type {
            err!(self.current_line_num, IRError::TypeMismatch(expected_type.clone(), got_type.clone()))
        }

        Ok(())

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

    fn allocate_variable(&mut self, name: Name, data_type: DataType) -> Identifier {

        let size = data_type.get_size(&self.user_definitions);

        for id in self.next_identifier..self.next_identifier + size {
            self.ir.0.push(IRStatement::Alloc(Allocation::Variable(id)));
        }
        
        self.name_table.insert(name, (self.next_identifier, data_type));

        self.next_identifier += size;
        self.next_identifier - 1

    }

    fn resolve_accessor(&self, accessor: &Accessor) -> Result<(Identifier, DataType), BrainFricError> {

        let (id, mut data_type) = self.name_table.get(accessor.name.as_ref()).map_or_else(
            ||
                err!(self.current_line_num, IRError::UnknownIdentifier(accessor.name.clone())),
            |(id, data_type)|
                Ok((id, data_type.clone()))
        )?;

        // todo: add cases for special accessors

        let mut true_id = *id;

        for specifier in accessor.specifiers.iter() {
            match (data_type, specifier) {
                (DataType::Sequence(inner_type, len), Specifier::ConstIndex(idx)) => {

                    if *idx as usize >= len {
                        err!(self.current_line_num, IRError::OutOfBoundsAccess);
                    }

                    true_id += *idx as usize * inner_type.get_size(&self.user_definitions);
                    data_type = *inner_type;
                    
                }
                (DataType::String(len), Specifier::ConstIndex(idx)) => {

                    if *idx as usize >= len {
                        err!(self.current_line_num, IRError::OutOfBoundsAccess);
                    }

                    true_id += *idx as usize + 1;
                    data_type = DataType::Byte;
                    
                }
                (data_type_again, Specifier::Field(field_name)) => {

                    let Some((new_data_type, offset)) = self.get_field_data(&data_type_again, field_name.clone())
                    else {
                        todo!("bad access");
                    };
                    
                    true_id += offset;
                    data_type = new_data_type;

                }
                _ => todo!("bad access")
            }
        }

        Ok((true_id, data_type))

    }

    fn resolve_enum_variant(&self, enum_name: &Name, variant_name: &Name) -> Result<u8, BrainFricError> {

        let Some(&enum_id) = self.user_definitions.enum_name_table.get(enum_name) else {
            err!(self.current_line_num, IRError::UnknownEnum(enum_name.clone()));
        };

        let Some(&value) = self.user_definitions.enums[enum_id].0.get(variant_name) else {
            err!(self.current_line_num, IRError::UnknownEnumVariant(enum_name.clone(), variant_name.clone()));
        };

        Ok(value)

    }

    fn try_get_expression_enum_id(&self, expression: &Expression) -> Option<usize> {
        match expression {
            Expression::Access(accessor) => {

                let Ok((_, data_type)) = self.resolve_accessor(&accessor) else {
                    return None;
                };

                match data_type {
                    DataType::UserEnum(enum_id) => Some(enum_id),
                    _ => None
                }
            }
            Expression::EnumVariant(enum_name, _) => self.user_definitions.enum_name_table.get(enum_name).map(|enum_id| *enum_id),
            _ => None
        }
    }

    fn get_expression_type(&self, expression: &Expression) -> Result<DataType, BrainFricError> {
        Ok(match expression {
            Expression::Access(accessor) => {
                self.resolve_accessor(accessor)?.1
            }
            Expression::Add(expr1, expr2) | 
            Expression::Subtract(expr1, expr2) | 
            Expression::Multiply(expr1, expr2) | 
            Expression::Divide(expr1, expr2) => {

                let type1 = self.get_expression_type(expr1)?;
                let type2 = self.get_expression_type(expr2)?;

                if type1 != type2 {
                    err!(self.current_line_num, IRError::TypeMismatch(type1, type2));
                }
                else {
                    type1
                }
            }
            Expression::AsBool(_) | Expression::BoolLiteral(_) | Expression::And(_, _) | Expression::Or(_, _) | Expression::Not(_) | Expression::Equals(_, _) | Expression::NotEquals(_, _) | Expression::GreaterThan(_, _) | Expression::LessThan(_, _) | Expression::GreaterThanEqual(_, _) | Expression::LessThanEqual(_, _)
                => DataType::Bool,
            Expression::NumberLiteral(_) => DataType::NumberLiteral
        })
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

    fn do_clear(&mut self, id: Identifier) {
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

    fn do_write(&mut self, id: Identifier) {
        self.ir.0.push(IRStatement::WriteByte(id));
    }

    fn do_write_byte_seq(&mut self, id: Identifier, len: usize) {
        self.ir.0.push(IRStatement::WriteByteSequence(id, len));
    }

    fn do_write_string(&mut self, id: Identifier) {
        self.ir.0.push(IRStatement::WriteString(id));
    }

    fn do_write_byte_as_number(&mut self, id: Identifier) {
        
        let temp_block = self.allocate_temporary_block(6);
        self.ir.0.push(IRStatement::WriteByteAsNumber { id, temp_block });

        self.free_block(temp_block, 6);
    }

    fn do_read(&mut self, id: Identifier) {
        self.ir.0.push(IRStatement::ReadByte(id));
    }

    fn do_copy_short(&mut self, to_low: Identifier, from_low: Identifier, negate: bool) {

        with_temp!(self, temp, {

            let to_high = to_low + 1;
            let from_high = from_low + 1;
    
            self.do_move([temp, to_high], from_high, false);
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

    fn evaluate_bool_expression_into(&mut self, expression: Expression, into: Identifier) -> Result<(), BrainFricError> {

        match expression {

            Expression::Access(accessor) => {
                let (id, data_type) = self.resolve_accessor(&accessor)?;
                self.assert_data_type(&DataType::Bool, &data_type)?;
                self.do_copy(into, id, false);
            }
            Expression::BoolLiteral(value) => {
                self.do_add_const(into, value as u8);
            }
            Expression::AsBool(expr) => {

                let (id, is_temp) = self.evaluate_expression(*expr, &DataType::Byte)?;

                with_temp!(self, temp, {

                    self.do_move([into, temp], id, false);
                    self.do_move([id], into, false);

                    do_if!(self, temp, {
                        self.do_add_const(into, 1);
                    });
                });

                self.try_free_access(id, is_temp);

            }
            Expression::Not(expr) => {

                with_temp!(self, temp, {

                    self.evaluate_bool_expression_into(*expr, temp)?;

                    self.do_add_const(into, 1);

                    do_if!(self, temp, {
                        self.do_sub_const(into, 1);
                    });
                });
            }
            Expression::And(expr1, expr2) => {

                with_temp!(self, temp1, temp2, temp3, {

                    self.evaluate_bool_expression_into(*expr1, temp1)?;
                    self.evaluate_bool_expression_into(*expr2, temp2)?;

                    self.do_move([temp3], temp2, false);

                    do_if!(self, temp3, {
                        self.do_move([into], temp1, false);
                    });
                });
            }
            Expression::Or(expr1, expr2) => {

                with_temp!(self, temp1, temp2, temp3, {

                    self.evaluate_bool_expression_into(*expr1, temp1)?;
                    self.evaluate_bool_expression_into(*expr2, temp2)?;
    
                    self.do_move([temp3], temp1, false);
                    self.do_move([temp3], temp2, false);

                    do_if!(self, temp3, {
                        self.do_add_const(into, 1);
                    });
                });
            }
            Expression::Equals(expr1, expr2) => {

                self.do_add_const(into, 1);

                with_temp!(self, temp, {

                    if let Some(enum_id) = self.try_get_expression_enum_id(&expr1) {

                        self.evaluate_enum_expression_into(*expr1, temp, enum_id, false)?;
                        self.evaluate_enum_expression_into(*expr2, temp, enum_id, true)?;
    
                        do_if!(self, temp, {
                            self.do_sub_const(into, 1);
                        });
                    }
                    else {

                        self.evaluate_byte_expression_into(*expr1, temp, false)?;
                        self.evaluate_byte_expression_into(*expr2, temp, true)?;
    
                        do_if!(self, temp, {
                            self.do_sub_const(into, 1);
                        });
                    }
                });
            }
            Expression::NotEquals(expr1, expr2) => {

                with_temp!(self, temp, {

                    self.evaluate_byte_expression_into(*expr1, temp, false)?;
                    self.evaluate_byte_expression_into(*expr2, temp, true)?;

                    do_if!(self, temp, {
                        self.do_add_const(into, 1);
                    });
                });
            }
            Expression::GreaterThan(expr1, expr2) => {

                with_temp!(self, temp1, temp2, temp3, {

                    self.evaluate_byte_expression_into(*expr1, temp1, false)?;
                    self.evaluate_byte_expression_into(*expr2, temp2, false)?;
    
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
            Expression::LessThan(expr1, expr2) => {

                with_temp!(self, temp1, temp2, temp3, {

                    self.evaluate_byte_expression_into(*expr1, temp1, false)?;
                    self.evaluate_byte_expression_into(*expr2, temp2, false)?;

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
            Expression::GreaterThanEqual(expr1, expr2) => {

                with_temp!(self, temp1, temp2, temp3, {

                    self.evaluate_byte_expression_into(*expr1, temp1, false)?;
                    self.evaluate_byte_expression_into(*expr2, temp2, false)?;

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
            Expression::LessThanEqual(expr1, expr2) => {

                with_temp!(self, temp1, temp2, temp3, {

                    self.evaluate_byte_expression_into(*expr1, temp1, false)?;
                    self.evaluate_byte_expression_into(*expr2, temp2, false)?;

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
            _ => err!(self.current_line_num, IRError::ExpectedTypedExpression(DataType::Bool))
        }

        Ok(())

    }

    fn evaluate_byte_expression_into(&mut self, expression: Expression, into: Identifier, negate: bool) -> Result<(), BrainFricError> {

        match expression {

            Expression::Access(accessor) => {
                let (id, data_type) = self.resolve_accessor(&accessor)?;
                self.assert_data_type(&DataType::Byte, &data_type)?;
                self.do_copy(into, id, negate);
            }
            Expression::AsNum(expr) => {
                self.evaluate_bool_expression_into(*expr, into)?;
            }
            Expression::NumberLiteral(value) => {
                if negate {
                    self.do_sub_const(into, value as u8);
                }
                else {
                    self.do_add_const(into, value as u8);
                }
            }
            Expression::Add(expr1, expr2) => {
                self.evaluate_byte_expression_into(*expr1, into, negate)?;
                self.evaluate_byte_expression_into(*expr2, into, negate)?;
            }
            Expression::Subtract(expr1, expr2) => {
                self.evaluate_byte_expression_into(*expr1, into, negate)?;
                self.evaluate_byte_expression_into(*expr2, into, !negate)?;
            }
            Expression::Multiply(expr1, expr2) => {

                with_temp!(self, temp1, temp2, temp3, {

                    self.evaluate_byte_expression_into(*expr1, temp1, false)?;
                    self.evaluate_byte_expression_into(*expr2, temp2, false)?;

                    do_while!(self, temp2, {

                        self.do_move_raw([(into, negate), (temp3, false)], temp1);
                        self.do_move([temp1], temp3, false);
    
                        self.do_sub_const(temp2, 1);

                    });
                });
            }
            _ => err!(self.current_line_num, IRError::ExpectedTypedExpression(DataType::Byte))
        };

        Ok(())

    }

    fn evaluate_short_expression_into(&mut self, expression: Expression, into: Identifier, negate: bool) -> Result<(), BrainFricError> {

        match expression {
            Expression::Access(accessor) => {
                let (id, data_type) = self.resolve_accessor(&accessor)?;
                self.assert_data_type(&DataType::Short, &data_type)?;
                self.do_copy_short(into, id, negate);
            }
            Expression::NumberLiteral(value) => {
                if negate {
                    self.do_sub_const_short(into, value as u16);
                }
                else {
                    self.do_add_const_short(into, value as u16);
                }
            }
            Expression::Add(expr1, expr2) => {
                self.evaluate_short_expression_into(*expr1, into, negate)?;
                self.evaluate_short_expression_into(*expr2, into, negate)?;
            }
            Expression::Subtract(expr1, expr2) => {
                self.evaluate_short_expression_into(*expr1, into, negate)?;
                self.evaluate_short_expression_into(*expr2, into, !negate)?;
            }
            Expression::Multiply(expr1, expr2) => {

                todo!("bruh moment")

            }
            _ => err!(self.current_line_num, IRError::ExpectedTypedExpression(DataType::Short))
        }

        Ok(())

    }

    fn evaluate_enum_expression_into(&mut self, expression: Expression, into: Identifier, enum_id: usize, negate: bool) -> Result<(), BrainFricError> {
        
        match expression {
            Expression::Access(accessor) => {
                let (id, data_type) = self.resolve_accessor(&accessor)?;
                self.assert_data_type(&DataType::UserEnum(enum_id), &data_type)?;
                self.do_copy(into, id, negate);
            }
            Expression::EnumVariant(enum_name, variant_name) => {

                let value = self.resolve_enum_variant(&enum_name, &variant_name)?;

                if negate {
                    self.do_sub_const(into, value);
                }
                else {
                    self.do_add_const(into, value);
                }
            }
            _ => err!(self.current_line_num, IRError::ExpectedTypedExpression(DataType::UserEnum(enum_id)))
        }

        Ok(())

    }

    fn evaluate_expression_into(&mut self, expression: Expression, expected_type: &DataType, into: Identifier) -> Result<(), BrainFricError> {

        match expected_type {
            DataType::Bool =>
                self.evaluate_bool_expression_into(expression, into),
            DataType::Byte =>
                self.evaluate_byte_expression_into(expression, into, false),
            DataType::Short =>
                self.evaluate_short_expression_into(expression, into, false),
            DataType::UserEnum(enum_id) => 
                self.evaluate_enum_expression_into(expression, into, *enum_id, false),
            _ => todo!()
        }
    }

    fn evaluate_expression(&mut self, expression: Expression, expected_type: &DataType) -> Result<(Identifier, bool), BrainFricError> {
        
        Ok(match expression {
            Expression::Access(accessor) => {
                let (mem, data_type) = self.resolve_accessor(&accessor)?;
                self.assert_data_type(expected_type, &data_type)?;
                (mem, false)
            }
            _ => {
                let mem = self.allocate_temporary();
                self.evaluate_expression_into(expression, expected_type, mem)?;
                (mem, true)
            }
        })
    }
    
    pub fn generate_ir(&mut self, mut statements: Vec<Statement>) -> Result<IRBlock, BrainFricError> {

        statements.reverse();
        
        while let Some(statement) = statements.pop() {

            self.current_line_num = statement.line_num;
            
            match statement.body {

                StatementBody::Declaration(names, parsed_data_type) => {

                    let data_type = DataType::convert_parsed(&parsed_data_type, statement.line_num, &self.user_definitions)?;

                    for name in names {
                        self.allocate_variable(name, data_type.clone());
                    }
                }
                StatementBody::Assign(accessor, expression) => {

                    let name = accessor.name.clone();
                    let (mem, data_type) = self.resolve_accessor(&accessor)?;

                    if let (
                        DataType::Sequence(box DataType::Byte, seq_size) | DataType::String(seq_size),
                        Expression::StringLiteral(string)
                    ) = (&data_type, &expression) {

                        let len = string.chars().count();

                        if len > *seq_size {
                            err!(self.current_line_num, ParseError::StringLiteralTooLarge);
                        }

                        let type_offset = if let DataType::Sequence(_, _) = data_type {0} else {1};

                        for (char_num, char) in string.chars().enumerate() {

                            let with_offset = mem + char_num + type_offset;

                            self.do_clear(with_offset);
                            self.do_add_const(with_offset, char as u8);

                        }

                        continue;

                    }

                    if expression.contains_name(&name) {

                        match data_type {
                            DataType::Bool | DataType::Byte => with_temp!(self, temp, {
                                self.evaluate_expression_into(expression, &data_type, temp)?;
                                self.do_clear(mem);
                                self.do_move([mem], temp, false);
                            }),
                            DataType::Short => with_temp!(self, lower, upper, {
                                self.evaluate_short_expression_into(expression, lower, false)?;
                                self.do_clear_short(mem);
                                self.do_move([mem], lower, false);
                                self.do_move([mem + 1], upper, false);
                            }),
                            _ => todo!()
                        }
                    }
                    else {

                        match data_type {
                            DataType::Short => self.do_clear_short(mem),
                            _ => self.do_clear(mem)
                        }

                        self.evaluate_expression_into(expression, &data_type, mem)?;

                    }
                }
                StatementBody::Inc(accessor) => {

                    let (mem, data_type) = self.resolve_accessor(&accessor)?;
                    self.assert_data_type(&data_type, &DataType::Byte)?;
                    self.do_add_const(mem, 1);

                }
                StatementBody::Dec(accessor) => {

                    let (mem, data_type) = self.resolve_accessor(&accessor)?;
                    self.assert_data_type(&data_type, &DataType::Byte)?;
                    self.do_sub_const(mem, 1);

                }
                StatementBody::Clear(accessor) => {

                    let (mem, data_type) = self.resolve_accessor(&accessor)?;

                    if !matches!(data_type, DataType::Sequence(_, _)) {
                        err!(self.current_line_num, IRError::ExpectedSequence);
                    }
                    
                    // make more sophisticated

                    for cell in 0..data_type.get_size(&self.user_definitions) {
                        self.do_clear(mem + cell);
                    }
                }
                StatementBody::LeftShift(accessor, amount) => {
                    todo!()
                }
                StatementBody::RightShift(accessor, amount) => {
                    todo!()
                }
                StatementBody::Write(expression) => {

                    if let Expression::StringLiteral(literal) = expression {

                        with_temp!(self, temp, {

                            let mut val = 0u8;
    
                            for chr in literal.chars() {
                                self.do_add_const(temp, (chr as u8).wrapping_sub(val));
                                self.do_write(temp);
                                val = chr as u8;
                            }
                        });
                        
                        continue;
                    
                    }
                    
                    if let Expression::Access(access) = &expression {

                        let (mem, data_type) = self.resolve_accessor(&access.clone())?;

                        if let DataType::Sequence(box DataType::Byte, len) = data_type {
                            self.do_write_byte_seq(mem, len);
                            continue;
                        }
                        else if let DataType::String(_) = data_type {
                            self.do_write_string(mem);
                            continue;
                        }
                    }

                    let (mem, is_temp) = self.evaluate_expression(expression, &DataType::Byte)?;
                    self.do_write(mem);
                    self.try_free_access(mem, is_temp);

                }
                StatementBody::WriteNum(expression) => {

                    let (mem, is_temp) = self.evaluate_expression(expression, &DataType::Byte)?;
                    self.do_write_byte_as_number(mem);

                    self.try_free_access(mem, is_temp);

                }
                StatementBody::WriteLine => {
                    with_temp!(self, temp, {
                        self.do_add_const(temp, 10);
                        self.do_write(temp);
                    });
                }
                StatementBody::Read(accessor) => {
                    let (access, data_type) = self.resolve_accessor(&accessor)?;
                    self.assert_data_type(&data_type, &DataType::Byte)?;
                    self.do_read(access);
                }
                StatementBody::While(expression, loop_statements) => {

                    // optimization for special case
                    // also add for bool variable??
                    if let Expression::AsBool(box Expression::Access(accessor)) = expression {

                        let (mem, data_type) = self.resolve_accessor(&accessor)?;
                        self.assert_data_type(&data_type, &DataType::Bool)?;

                        self.push_ir_to_stack();
                        let loop_ir = self.generate_ir(loop_statements)?;
                        self.ir.0.extend(loop_ir.0);
                        self.do_end_loop(mem, false);

                        continue;

                    }

                    let (mem1, is_temp1) = self.evaluate_expression(expression.clone(), &DataType::Bool)?;

                    do_while!(self, mem1, {
    
                        let loop_ir = self.generate_ir(loop_statements)?;
                        self.ir.0.extend(loop_ir.0);
    
                        let (mem2, is_temp2) = self.evaluate_expression(expression, &DataType::Bool)?;
    
                        if mem1 != mem2 {
                            self.do_clear(mem1);
                            self.do_move([mem1], mem2, false);
                        }

                        self.try_free_access(mem2, is_temp2);

                    });

                    self.try_free_access(mem1, is_temp1);

                }
                StatementBody::If(expression, loop_statements, else_block) => {

                    with_temp!(self, temp, {

                        self.evaluate_bool_expression_into(expression, temp)?;

                        if let Some(block) = else_block {

                            with_temp!(self, temp2, {

                                self.do_add_const(temp2, 1);

                                do_if!(self, temp, {
        
                                    let if_ir = self.generate_ir(loop_statements)?;
                                    self.ir.0.extend(if_ir.0);
                
                                    self.do_sub_const(temp2, 1);
                                    
                                });

                                do_if!(self, temp2, {
                                    let else_ir = self.generate_ir(block)?;
                                    self.ir.0.extend(else_ir.0);
                                });
                            });
                        }
                        else {
                            do_if!(self, temp, {
                                let loop_ir = self.generate_ir(loop_statements)?;
                                self.ir.0.extend(loop_ir.0);
                            });
                        }
                    });
                }
                StatementBody::Switch(expression, arms, default_statements) => {

                    let mut arm_values = arms
                        .into_iter()
                        .map(|(arm, statements)| {
                            Ok((match arm {
                                MatchArm::NumberLiteral(val) => val,
                                MatchArm::EnumVariant(enum_name, variant_name) => self.resolve_enum_variant(&enum_name, &variant_name)?
                            }, statements))
                        })
                        .collect::<Result<Vec<_>, _>>()?;

                    let temp_block = self.allocate_temporary_block(2);

                    let data_type = match self.try_get_expression_enum_id(&expression) {
                        Some(enum_id) => DataType::UserEnum(enum_id),
                        None => DataType::Byte
                    };

                    self.evaluate_expression_into(expression, &data_type, temp_block + 1)?;

                    arm_values.sort_by(|(case1, _), (case2, _)| {
                        case1.cmp(case2)
                    });

                    self.push_ir_to_stack();

                    let arm_ir_blocks: Result<Vec<(u8, IRBlock)>, BrainFricError> = arm_values.into_iter().map(|(case, block)| {

                        if case == 0 {
                            panic!("zero case not supported yet");
                        }

                        let ir = self.generate_ir(block)?;
                        Ok((case, ir))
                        
                    }).collect();

                    let default = match default_statements {
                        Some(statements) => {
                            Some(self.generate_ir(statements)?)
                        }
                        None => None
                    };

                    self.pop_ir_from_stack();

                    self.ir.0.push(IRStatement::Switch {
                        temp_block,
                        arms: arm_ir_blocks?,
                        default
                    });

                    self.free_block(temp_block, 2);
                    
                }
                _ => todo!("unimplemented statement type")
            }
        }

        Ok(take(&mut self.ir))
        
    }
}