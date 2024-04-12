use std::collections::{HashMap, HashSet};
use std::mem::{swap, take, replace};
use std::num::NonZeroUsize;

use crate::args::arg_allow_delete_variables;
use crate::error::*;
use crate::err;
use crate::lex::Name;
use crate::parse::*;

// pub trait Variable {
//     fn get_known_value(&self) -> Option<u8>;
//     fn clear_value(&mut self);
//     fn try_as_byte(&self) -> Option<&Byte> {None}
// }

// pub struct Byte {
//     known_value: Option<u8>
// }

// impl Variable for Byte {

//     fn get_known_value(&self) -> Option<u8> {
//         self.known_value
//     }

//     fn clear_value(&mut self) {
//         self.known_value = None;
//     }

//     fn try_as_byte(&self) -> Option<&Byte> {
//         Some(&self)
//     }
// }

// pub struct Short {
//     known_value: Option<u16>
// }

// impl Variable for Short {

//     fn get_known_value(&self) -> Option<u8> {
//         todo!()
//     }

//     fn clear_value(&mut self) {
//         self.known_value = None;
//     }
// }

// pub struct Sequence<const L: usize, T: Variable> {
//     values: [T; L]
// }

// impl<const L: usize, T: Variable> Variable for Sequence<L, T> {

//     fn get_known_value(&self) -> Option<u8> {
//         todo!()
//     }

//     fn clear_value(&mut self) {
//         for value in &mut self.values {
//             value.clear_value();  
//         }
//     }
// }

// pub enum _Allocation {
//     Variable(Box<dyn Variable>),
//     Temporary(Byte)
// }

// pub struct _Identifier {
//     pub identifier: Rc<RefCell<dyn Variable>>,
//     pub offsets: Box<[Offset]>
// }

// pub enum _IRStatement {
//     Alloc(_Allocation),
//     Free(Identifier),
//     AddConst(_Identifier, u8),
//     MoveCell(Box<[(Identifier, bool)]>, Identifier),
//     WriteByte(Identifier),
//     WriteByteSequence(Identifier, usize),
//     WriteString(Identifier),
//     ReadByte(Identifier),
//     Loop(Identifier, IRBlock, bool)
// }

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DataType {
    Bool,
    Byte,
    Short,
    Sequence(Box<DataType>, usize),
    String(usize),
    Stack(Box<DataType>, usize),
    //Array(Rc<DataType>, usize)
}

impl DataType {

    pub fn convert_parsed(line_num: usize, parsed_data_type: &ParsedDataType) -> Result<Self, BrainFricError> {

        macro_rules! assert_param_types {
            ($data_type: ident, $expected: expr) => {
                {
                    
                    let num_params = vec![$expected].len();
                    
                    if num_params != parsed_data_type.parameters.len() {
                        err!(line_num, IRError::InvalidTypeParameters);
                    }
                    
                    for i in 0..num_params {
                        if $expected[i] != parsed_data_type.parameters[i].get_type_val() {
                            err!(line_num, IRError::InvalidTypeParameters);
                        }
                    }
                }
            }
        }

        const CONSTANT: u32 = 0;
        const TYPE: u32 = 1;
        const NONE: [u32; 0] = [];

        match parsed_data_type.head {
            DataTypeHead::Bool => {
                assert_param_types!(Bool, NONE);
                Ok(DataType::Bool)
            }
            DataTypeHead::Byte => {
                assert_param_types!(Byte, NONE);
                Ok(DataType::Bool)
            }
            DataTypeHead::Short => {
                assert_param_types!(Short, NONE);
                Ok(DataType::Bool)
            }
            
            DataTypeHead::Sequence => {

                assert_param_types!(Bool, [TYPE, CONSTANT]);

                let (DataTypeParameter::Type(data_type), DataTypeParameter::Constant(length)) = (&parsed_data_type.parameters[0], &parsed_data_type.parameters[1])
                else {
                    unreachable!();
                };

                Ok(DataType::Sequence(Box::new(Self::convert_parsed(line_num, data_type)?), *length))

            }
            DataTypeHead::String => {
            
                assert_param_types!(String, [CONSTANT]);

                let DataTypeParameter::Constant(length) = &parsed_data_type.parameters[0]
                else {
                    unreachable!();
                };

                Ok(DataType::String(*length))

            }
            DataTypeHead::Stack => {

                assert_param_types!(Bool, [TYPE, CONSTANT]);

                let (DataTypeParameter::Type(data_type), DataTypeParameter::Constant(length)) = (&parsed_data_type.parameters[0], &parsed_data_type.parameters[1])
                else {
                    unreachable!();
                };

                Ok(DataType::Stack(Box::new(Self::convert_parsed(line_num, data_type)?), *length))

            }
        }
    }

    pub fn get_size(&self) -> usize {

        match &self {
            Self::Bool => 1,
            Self::Byte => 1,
            Self::Short => 2,
            Self::Sequence(data_type, len) => len * data_type.get_size(),
            Self::String(len) => len + 2,
            Self::Stack(data_type, len) => len * (data_type.get_size() + 1) + 1,
            //Self::Array(data_type, len) => data_type.get_size() * len
        }
    }
}

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
            write!(f, "\n  ")?;
            statement.fmt(f)
        });
        
        writeln!(f)?;
        err

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
    Loop(Identifier, IRBlock, bool)
}

pub fn generate_ir(statements: Vec<Statement>) -> Result<IRBlock, BrainFricError> {

    let mut ir_generator = IRGenerator {
        loop_stack: Vec::new(),
        ir: IRBlock::new(),
        current_line_num: 0,
        next_identifier: 1,
        name_table: HashMap::new()
    };

    ir_generator.generate_ir(statements)
    
}

struct IRGenerator {
    ir: IRBlock,
    loop_stack: Vec<IRBlock>,
    current_line_num: usize,
    next_identifier: Identifier,
    name_table: HashMap<Name, (Identifier, DataType)>
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

    fn do_free_access(&mut self, mem: Identifier) {
        self.do_free(mem);
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

        for i in 0..size {
            self.ir.0.push(IRStatement::Alloc(Allocation::Temporary(self.next_identifier)));
            self.next_identifier += 1;
        }

        id

    }

    fn allocate_variable(&mut self, name: Name, data_type: DataType) -> Identifier {

        let size = data_type.get_size();

        for id in self.next_identifier..self.next_identifier + size {
            self.ir.0.push(IRStatement::Alloc(Allocation::Variable(id)));
        }
        
        self.name_table.insert(name, (self.next_identifier, data_type));

        self.next_identifier += size;
        self.next_identifier - 1

    }

    fn resolve_accessor(&self, accessor: Accessor) -> Result<(Identifier, DataType), BrainFricError> {

        let (id, mut data_type) = self.name_table.get(accessor.name.as_ref()).map_or_else(
            ||
                err!(self.current_line_num, IRError::UnknownIdentifier(accessor.name)),
            |(id, data_type)|
                Ok((id, data_type.clone()))
        )?;

        // todo: add cases for special accessors

        let mut true_id = *id;

        for specifier in accessor.specifiers.iter() {
            match (specifier, data_type) {
                (Specifier::ConstIndex(idx), DataType::Sequence(inner_type, len)) => {

                    if *idx as usize >= len {
                        err!(self.current_line_num, IRError::OutOfBoundsAccess);
                    }

                    true_id += *idx as usize * inner_type.get_size();
                    data_type = *inner_type;
                    
                }
                (Specifier::ConstIndex(idx), DataType::String(len)) => {

                    if *idx as usize >= len {
                        err!(self.current_line_num, IRError::OutOfBoundsAccess);
                    }

                    true_id += *idx as usize + 1;
                    data_type = DataType::Byte;
                    
                }
                (Specifier::Lower, DataType::Short) => {
                    data_type = DataType::Byte;
                }
                (Specifier::Upper, DataType::Short) => {
                    true_id += 1;
                    data_type = DataType::Byte;
                }
                _ => todo!("bad access")
            }
        }

        Ok((true_id, data_type))
    }

    fn do_move_raw<const N: usize>(&mut self, to: [(Identifier, bool); N], from: Identifier) {
        self.ir.0.push(IRStatement::MoveCell(
            to.iter()
                .map(|(mem, negate)| (mem.clone(), *negate))
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
        let temp = self.allocate_temporary();
        self.do_move_raw([(temp.clone(), false), (to, negate)], from.clone());
        self.do_move([from], temp.clone(), false);
        self.do_free(temp);
    }

    fn do_clear(&mut self, id: Identifier) {
        self.ir.0.push(IRStatement::MoveCell(Box::default(), id));
    }

    fn do_add_const(&mut self, id: Identifier, val: u8) {
        self.ir.0.push(IRStatement::AddConst(id, val));
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

        let temp = self.allocate_temporary();
        let to_high = to_low + 1;
        let from_high = from_low + 1;

        self.do_move([temp.clone(), to_low], from_low.clone(), false);
        self.do_move([from_low], temp.clone(), negate);

        self.do_move([temp.clone(), to_high], from_high.clone(), false);
        self.do_move([from_high], temp.clone(), negate);

        self.do_free(temp);

    }

    fn do_add_const_short(&mut self, id: Identifier, val: u16) {
        self.do_add_const(id, (val & 0xff) as u8);
        self.do_add_const(id + 1, (val >> 8) as u8);
    }

    fn do_sub_const_short(&mut self, id: Identifier, val: u16) {
        self.do_add_const_short(id, val.wrapping_neg());
    }

    fn do_clear_short(&mut self, id: Identifier) {
        self.ir.0.push(IRStatement::MoveCell(Box::default(), id));
        self.ir.0.push(IRStatement::MoveCell(Box::default(), id + 1));
    }

    fn do_begin_loop(&mut self) {
        self.loop_stack.push(take(&mut self.ir));
    }

    fn do_end_loop(&mut self, mem: Identifier, is_if: bool) {
        swap(&mut self.ir, self.loop_stack.last_mut().unwrap());
        self.ir.0.push(IRStatement::Loop(mem, self.loop_stack.pop().unwrap(), is_if));
    }

    fn evaluate_bool_expression_into(&mut self, expression: Expression, into: Identifier) -> Result<(), BrainFricError> {

        match expression {

            Expression::Access(accessor) => {
                let (mem, data_type) = self.resolve_accessor(accessor)?;
                self.assert_data_type(&DataType::Bool, &data_type)?;
                self.do_copy(into, mem, false);
            }
            Expression::BoolLiteral(value) => {
                self.do_add_const(into, value as u8);
            }
            Expression::AsBool(expr) => {

                let (mem, is_temp) = self.evaluate_expression(*expr, &DataType::Byte)?;
                let temp = self.allocate_temporary();

                self.do_move([into.clone(), temp.clone()], mem.clone(), false);
                self.do_move([mem.clone()], into.clone(), false);

                self.do_begin_loop();
                self.do_add_const(into, 1);
                self.do_clear(temp.clone());
                self.do_end_loop(temp.clone(), true);
                
                self.do_free(temp);
                self.try_free_access(mem, is_temp);

            }
            Expression::Not(expr) => {

                let temp = self.allocate_temporary();
                self.evaluate_bool_expression_into(*expr, temp.clone())?;

                self.do_add_const(into.clone(), 1);

                self.do_begin_loop();
                self.do_sub_const(temp.clone(), 1);
                self.do_sub_const(into, 1);
                self.do_end_loop(temp.clone(), true);

                self.do_free(temp);

            }
            Expression::And(expr1, expr2) => {

                let (mem1, is_temp1) = self.evaluate_expression(*expr1, &DataType::Bool)?;
                let (mem2, is_temp2) = self.evaluate_expression(*expr2, &DataType::Bool)?;

                let temp = self.allocate_temporary();

                self.do_copy(temp.clone(), mem2.clone(), false);

                self.do_begin_loop();
                self.do_copy(into, mem1.clone(), false);
                self.do_sub_const(temp.clone(), 1);
                self.do_end_loop(temp.clone(), true);

                self.do_free(temp);
                self.try_free_access(mem2, is_temp2);
                self.try_free_access(mem1, is_temp1);

            }
            Expression::Or(expr1, expr2) => {

                let (mem1, is_temp1) = self.evaluate_expression(*expr1, &DataType::Bool)?;
                let (mem2, is_temp2) = self.evaluate_expression(*expr2, &DataType::Bool)?;

                let temp = self.allocate_temporary();
                self.do_copy(temp.clone(), mem1.clone(), false);
                self.do_copy(temp.clone(), mem2.clone(), false);

                self.do_begin_loop();
                self.do_add_const(into, 1);
                self.do_clear(temp.clone());
                self.do_end_loop(temp.clone(), true);

                self.do_free(temp);
                self.try_free_access(mem2, is_temp2);
                self.try_free_access(mem1, is_temp1);

            }
            Expression::Equals(expr1, expr2) => {

                self.do_add_const(into.clone(), 1);

                let temp1 = self.allocate_temporary();
                self.evaluate_expression_into(*expr1, &DataType::Byte, temp1.clone())?;

                let temp2 = self.allocate_temporary();
                self.evaluate_expression_into(*expr2, &DataType::Byte, temp2.clone())?;

                self.do_move([temp2.clone()], temp1, true);

                self.do_begin_loop();
                self.do_sub_const(into, 1);
                self.do_clear(temp2.clone());
                self.do_end_loop(temp2, true);

            }
            Expression::NotEquals(expr1, expr2) => {

                let temp1 = self.allocate_temporary();
                self.evaluate_expression_into(*expr1, &DataType::Byte, temp1.clone())?;

                let temp2 = self.allocate_temporary();
                self.evaluate_expression_into(*expr2, &DataType::Byte, temp2.clone())?;

                self.do_move([temp2.clone()], temp1, true);

                self.do_begin_loop();
                self.do_add_const(into, 1);
                self.do_clear(temp2.clone());
                self.do_end_loop(temp2, true);

            }
            _ => err!(self.current_line_num, IRError::ExpectedTypedExpression(DataType::Bool))
        };

        Ok(())

    }

    fn evaluate_byte_expression_into(&mut self, expression: Expression, into: Identifier, negate: bool) -> Result<(), BrainFricError> {

        match expression {

            Expression::Access(accessor) => {
                let (mem, data_type) = self.resolve_accessor(accessor)?;
                self.assert_data_type(&DataType::Byte, &data_type)?;
                self.do_copy(into, mem, negate);
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
                self.evaluate_byte_expression_into(*expr1, into.clone(), negate)?;
                self.evaluate_byte_expression_into(*expr2, into, negate)?;
            }
            Expression::Subtract(expr1, expr2) => {
                self.evaluate_byte_expression_into(*expr1, into.clone(), negate)?;
                self.evaluate_byte_expression_into(*expr2, into, !negate)?;
            }
            Expression::Multiply(expr1, expr2) => {

                let temp1 = self.allocate_temporary();
                self.evaluate_byte_expression_into(*expr1, temp1.clone(), false)?;

                let temp2 = self.allocate_temporary();
                self.evaluate_byte_expression_into(*expr2, temp2.clone(), false)?;

                let temp3 = self.allocate_temporary();

                self.do_begin_loop();

                self.do_move_raw([(into, negate), (temp3.clone(), false)], temp1.clone());

                self.do_move([temp1.clone()], temp3.clone(), false);

                self.do_sub_const(temp2.clone(), 1);
                self.do_end_loop(temp2.clone(), false);

                self.do_free_access(temp3);
                self.do_free_access(temp2);
                self.do_free_access(temp1);

            }
            _ => err!(self.current_line_num, IRError::ExpectedTypedExpression(DataType::Byte))
        };

        Ok(())

    }

    fn evaluate_short_expression_into(&mut self, expression: Expression, into: Identifier, negate: bool) -> Result<(), BrainFricError> {

        match expression {
            Expression::Access(accessor) => {
                let (mem, data_type) = self.resolve_accessor(accessor)?;
                self.assert_data_type(&DataType::Short, &data_type)?;
                self.do_copy_short(into, mem, negate);
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

                // todo, negative case
                assert!(negate == false);

                self.evaluate_short_expression_into(*expr1, into.clone(), negate)?;

                let old_lower_copy = self.allocate_temporary();
                self.do_copy(old_lower_copy.clone(), into.clone(), false);

                self.evaluate_short_expression_into(*expr2, into.clone(), negate)?;

                let new_lower_copy = self.allocate_temporary();
                self.do_copy(new_lower_copy.clone(), into.clone(), false);

                let temp = self.allocate_temporary();
                let into_upper = into + 1;

                self.do_begin_loop();

                self.do_sub_const(old_lower_copy.clone(), 1);
                self.do_sub_const(new_lower_copy.clone(), 1);

                self.do_move([temp.clone()], new_lower_copy.clone(), false);

                self.do_begin_loop();
                self.do_move([new_lower_copy.clone()], temp.clone(), false);
                self.do_sub_const(into_upper.clone(), 1);
                self.do_end_loop(temp, true);

                self.do_add_const(into_upper, 1);

                self.do_end_loop(old_lower_copy, false);

                self.do_clear(new_lower_copy);

            }
            Expression::Subtract(expr1, expr2) => {

                todo!("not done yet");

                // todo, negative case
                assert!(negate == false);

                self.evaluate_short_expression_into(*expr1, into.clone(), negate)?;

                let old_lower_copy = self.allocate_temporary();
                self.do_copy(old_lower_copy.clone(), into.clone(), false);

                self.evaluate_short_expression_into(*expr2, into.clone(), true)?;

                let new_lower_copy = self.allocate_temporary();
                self.do_copy(new_lower_copy.clone(), into.clone(), false);

                let temp = self.allocate_temporary();
                let into_upper = into + 1;

                self.do_begin_loop();

                self.do_sub_const(old_lower_copy.clone(), 1);
                self.do_sub_const(new_lower_copy.clone(), 1);

                self.do_move([temp.clone()], old_lower_copy.clone(), false);

                self.do_begin_loop();
                self.do_move([old_lower_copy.clone()], temp.clone(), false);
                self.do_add_const(into_upper.clone(), 1);
                self.do_end_loop(temp, true);

                self.do_sub_const(into_upper, 1);

                self.do_end_loop(new_lower_copy, false);

                self.do_clear(old_lower_copy);

            }
            _ => err!(self.current_line_num, IRError::ExpectedTypedExpression(DataType::Short))
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
            _ => todo!()
        }
    }

    fn evaluate_expression(&mut self, expression: Expression, expected_type: &DataType) -> Result<(Identifier, bool), BrainFricError> {
        
        Ok(match expression {
            Expression::Access(accessor) => {
                let (mem, data_type) = self.resolve_accessor(accessor)?;
                self.assert_data_type(expected_type, &data_type)?;
                (mem, false)
            }
            _ => {
                let mem = self.allocate_temporary();
                self.evaluate_expression_into(expression, expected_type, mem.clone())?;
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

                    let data_type = DataType::convert_parsed(statement.line_num, &parsed_data_type)?;

                    for name in names {
                        self.allocate_variable(name, data_type.clone());
                    }
                }
                StatementBody::SetTo(accessor, expression) => {

                    let (mem, data_type) = self.resolve_accessor(accessor)?;

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

                            self.do_clear(with_offset.clone());
                            self.do_add_const(with_offset, char as u8);

                        }

                        continue;

                    }

                    self.do_clear(mem.clone());
                    self.evaluate_expression_into(expression, &data_type, mem)?;

                }
                StatementBody::Inc(accessor) => {

                    let (mem, data_type) = self.resolve_accessor(accessor)?;
                    self.assert_data_type(&data_type, &DataType::Byte)?;
                    self.do_add_const(mem, 1);

                }
                StatementBody::Dec(accessor) => {

                    let (mem, data_type) = self.resolve_accessor(accessor)?;
                    self.assert_data_type(&data_type, &DataType::Byte)?;
                    self.do_sub_const(mem, 1);

                }
                StatementBody::Clear(accessor) => {

                    let (mem, data_type) = self.resolve_accessor(accessor)?;

                    if !matches!(data_type, DataType::Sequence(_, _)) {
                        err!(self.current_line_num, IRError::ExpectedSequence);
                    }
                    
                    // make more sophisticated

                    for cell in 0..data_type.get_size() {
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

                        let temp = self.allocate_temporary();

                        let mut val = 0u8;

                        for chr in literal.chars() {
                            self.do_add_const(temp.clone(), (chr as u8).wrapping_sub(val));
                            self.do_write(temp.clone());
                            val = chr as u8;
                        }

                        self.do_free_access(temp);
                        continue;
                    
                    }
                    
                    if let Expression::Access(access) = &expression {

                        let (mem, data_type) = self.resolve_accessor(access.clone())?;

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
                    self.do_write(mem.clone());
                    self.try_free_access(mem, is_temp);

                }
                StatementBody::WriteNum(expression) => {

                    let (mem, is_temp) = self.evaluate_expression(expression, &DataType::Byte)?;
                    self.do_write_byte_as_number(mem);

                    self.try_free_access(mem, is_temp);

                }
                StatementBody::Read(accessor) => {
                    let (access, data_type) = self.resolve_accessor(accessor)?;
                    self.assert_data_type(&data_type, &DataType::Byte)?;
                    self.do_read(access);
                }
                StatementBody::While(expression, loop_statements) => {

                    // optimization for special case
                    // also add for bool variable??
                    if let Expression::AsBool(box Expression::Access(accessor)) = expression {

                        let (mem, data_type) = self.resolve_accessor(accessor)?;
                        self.assert_data_type(&data_type, &DataType::Bool)?;

                        self.do_begin_loop();
                        let loop_ir = self.generate_ir(loop_statements)?;
                        self.ir.0.extend(loop_ir.0);
                        self.do_end_loop(mem, false);

                        continue;

                    }

                    let (mem1, is_temp1) = self.evaluate_expression(expression.clone(), &DataType::Bool)?;

                    self.do_begin_loop();

                    let loop_ir = self.generate_ir(loop_statements)?;
                    self.ir.0.extend(loop_ir.0);

                    let (mem2, is_temp2) = self.evaluate_expression(expression, &DataType::Bool)?;

                    if mem1 != mem2 {
                        self.do_move([mem1.clone()], mem2.clone(), false);
                    }

                    self.do_end_loop(mem1.clone(), false);

                    self.try_free_access(mem2, is_temp2);
                    self.try_free_access(mem1, is_temp1);

                }
                StatementBody::If(expression, loop_statements) => {

                    let temp = self.allocate_temporary();
                    self.evaluate_bool_expression_into(expression, temp.clone())?;
                    
                    self.do_begin_loop();

                    let loop_ir = self.generate_ir(loop_statements)?;
                    self.ir.0.extend(loop_ir.0);

                    self.do_clear(temp.clone());
                    self.do_end_loop(temp.clone(), true);

                    self.do_free(temp);

                }
                _ => todo!()
            }
        }

        Ok(take(&mut self.ir))
        
    }
}