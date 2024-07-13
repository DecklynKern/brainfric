use std::collections::HashMap;
use std::rc::Rc;
use std::mem::{take, swap, replace};

use crate::lex::Name;
use crate::parse::*;
use crate::error::*;
use crate::err;

pub type NameID = usize;

struct UserEnum(pub HashMap<Name, u8>);

struct UserStruct {
    pub size: usize,
    pub fields: HashMap<Name, (ElaboratedDataType, usize)>
}

struct UserDefinitions {
    pub enum_name_table: HashMap<Name, usize>,
    pub enums: Vec<UserEnum>,
    pub struct_name_table: HashMap<Name, usize>,
    pub structs: Vec<UserStruct>
}

impl UserDefinitions {

    pub fn new(parsed_definitions: Vec<Definition>) -> Result<Self, BrainFricError> {

        let mut user_definitions = UserDefinitions {
            enum_name_table: HashMap::new(),
            enums: Vec::new(),
            struct_name_table: HashMap::new(),
            structs: Vec::new()
        };
    
        for parsed_definition in parsed_definitions {
            match parsed_definition {
                Definition::Enum(name, variants) => {

                    let mut user_enum_variants = HashMap::new();

                    for (variant_name, value) in variants {
                        user_enum_variants.insert(variant_name, value);
                    }
                    
                    user_definitions.enum_name_table.insert(name, user_definitions.enums.len());
                    user_definitions.enums.push(UserEnum(user_enum_variants));
                    
                }
                Definition::Struct(name, fields) => {
    
                    let mut offset = 0;
                    let mut user_struct_fields = HashMap::new();

                    for (field_name, parsed_data_type) in fields {
                        
                        let data_type = ElaboratedDataType::convert_parsed(&parsed_data_type, &user_definitions)?;
                    
                        let size = data_type.get_size(&user_definitions);
                        user_struct_fields.insert(field_name, (data_type, offset));
                        offset += size;
                    
                    }
    
                    user_definitions.struct_name_table.insert(name, user_definitions.structs.len());
                    user_definitions.structs.push(UserStruct {
                        size: offset,
                        fields: user_struct_fields
                    });
                }
            }
        }
    
        Ok(user_definitions)

    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ElaboratedDataType {
    Bool,
    Byte,
    Short,
    Fixed,
    Sequence(Box<ElaboratedDataType>, usize),
    GenericNumber,
    GenericString,
    Stack(Box<ElaboratedDataType>, usize),
    UserEnum(usize),
    UserStruct(usize)
    //Array(Rc<DataType>, usize)
}

impl ElaboratedDataType {

    fn can_conflate(&self, other: &Self) -> bool {

        self == other ||
        matches!(
            (self, other),
            (Self::GenericNumber, Self::Byte | Self::Short) |
            (Self::GenericString, Self::Sequence(box Self::Byte, _))
        )
    }

    fn get_subtype(arg: &DataTypeParameter) -> &ParsedDataType {
        match arg {
            DataTypeParameter::Constant(_) => unreachable!(),
            DataTypeParameter::Type(subtype) => subtype
        }
    }

    fn get_constant_val(arg: &DataTypeParameter) -> usize {
        match arg {
            DataTypeParameter::Constant(val) => *val,
            DataTypeParameter::Type(_) => unreachable!()
        }
    }

    fn get_size(&self, user_definitions: &UserDefinitions) -> usize {

        match &self {
            Self::Bool => 1,
            Self::Byte => 1,
            Self::Short => 2,
            Self::Fixed => 2,
            Self::Sequence(data_type, len) => len * data_type.get_size(user_definitions),
            Self::Stack(data_type, len) => (len + 1) * (data_type.get_size(user_definitions) + 1) + 1,
            Self::UserEnum(_) => 1,
            Self::UserStruct(struct_id) => user_definitions.structs[*struct_id].size,
            //Self::Array(data_type, len) => data_type.get_size() * len
            Self::GenericNumber | Self::GenericString => unreachable!()
        }
    }

    fn convert_parsed_simple(parsed_data_type: &ParsedDataType, user_definitions: &UserDefinitions) -> Result<Self, BrainFricError> {

        if !parsed_data_type.parameters.is_empty() {
            err!(ElaborateError::NestedComplexType);
        }

        Self::convert_parsed(parsed_data_type, user_definitions)

    }
    
    fn convert_parsed(parsed_data_type: &ParsedDataType, user_definitions: &UserDefinitions) -> Result<Self, BrainFricError> {

        let params = &parsed_data_type.parameters;

        const CONSTANT: u32 = 0;
        const TYPE: u32 = 1;

        let expected_types = match parsed_data_type.head {
            DataTypeHead::Sequence => vec![TYPE, CONSTANT],
            DataTypeHead::Stack => vec![TYPE, CONSTANT],
            _ => vec![]
        };

        if expected_types.len() != params.len() {
            err!(ElaborateError::InvalidTypeParameters);
        }

        for idx in 0..expected_types.len() {
            if expected_types[idx] != params[idx].get_type_val() {
                err!(ElaborateError::InvalidTypeParameters);
            }
        }

        Ok(match &parsed_data_type.head {
            DataTypeHead::Bool => Self::Bool,
            DataTypeHead::Byte => Self::Byte,
            DataTypeHead::Short => Self::Short,
            DataTypeHead::Fixed => Self::Fixed,
            DataTypeHead::Sequence => Self::Sequence(
                Box::new(Self::convert_parsed_simple(Self::get_subtype(&params[0]), user_definitions)?),
                Self::get_constant_val(&params[1])
            ),
            DataTypeHead::Stack => Self::Stack(
                Box::new(Self::convert_parsed_simple(Self::get_subtype(&params[0]), user_definitions)?),
                Self::get_constant_val(&params[1])
            ),
            DataTypeHead::Array => todo!(),
            DataTypeHead::UserDefined(name) => {

                match user_definitions.enum_name_table.get(name) {
                    Some(enum_id) => Self::UserEnum(*enum_id),
                    None => match user_definitions.struct_name_table.get(name) {
                        Some(struct_id) => Self::UserStruct(*struct_id),
                        None => err!(ElaborateError::UnknownType(name.clone()))
                    }
                }
            }
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ElaboratedSpecifier {
    ConstOffset(usize)
}

#[derive(PartialEq, Eq, Clone)]
pub struct ElaboratedAccessor {
    pub name_id: NameID,
    pub specifiers: Vec<ElaboratedSpecifier>
}

impl std::fmt::Debug for ElaboratedAccessor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ElaboratedAccessor({}, {:?})", self.name_id, self.specifiers)
    }
}

#[derive(Debug, Clone)]
pub enum BoolExpression {

    Access(ElaboratedAccessor),
    Constant(bool),

    ByteEquals(Box<ByteExpression>, Box<ByteExpression>),
    ByteNotEquals(Box<ByteExpression>, Box<ByteExpression>),
    ByteLessThan(Box<ByteExpression>, Box<ByteExpression>),
    ByteGreaterThan(Box<ByteExpression>, Box<ByteExpression>),
    ByteLessThanEqual(Box<ByteExpression>, Box<ByteExpression>),
    ByteGreaterThanEqual(Box<ByteExpression>, Box<ByteExpression>),

    ShortEquals(Box<ShortExpression>, Box<ShortExpression>),
    ShortNotEquals(Box<ShortExpression>, Box<ShortExpression>),
    ShortLessThan(Box<ShortExpression>, Box<ShortExpression>),
    ShortGreaterThan(Box<ShortExpression>, Box<ShortExpression>),
    ShortLessThanEqual(Box<ShortExpression>, Box<ShortExpression>),
    ShortGreaterThanEqual(Box<ShortExpression>, Box<ShortExpression>),

    Not(Box<BoolExpression>),
    And(Box<BoolExpression>, Box<BoolExpression>),
    Or(Box<BoolExpression>, Box<BoolExpression>),

    ConvertByte(Box<ByteExpression>),
    ConvertShort(Box<ShortExpression>)

}

#[derive(Debug, Clone)]
pub enum ByteExpression {

    Access(ElaboratedAccessor),

    Constant(u8),

    Add(Box<ByteExpression>, Box<ByteExpression>),
    Subtract(Box<ByteExpression>, Box<ByteExpression>),
    Multiply(Box<ByteExpression>, Box<ByteExpression>),
    Divide(Box<ByteExpression>, Box<ByteExpression>),

    ConvertBool(Box<BoolExpression>),
    ConvertShort(Box<ShortExpression>)

}

#[derive(Debug, Clone)]
pub enum ShortExpression {

    Access(ElaboratedAccessor),
    Constant(u16),

    Add(Box<ShortExpression>, Box<ShortExpression>),
    Subtract(Box<ShortExpression>, Box<ShortExpression>),
    Multiply(Box<ShortExpression>, Box<ShortExpression>),
    Divide(Box<ShortExpression>, Box<ShortExpression>),

}

#[derive(Debug)]
pub enum StringExpression {
    Constant(Rc<str>)
}

#[derive(Debug)]
pub enum ElaboratedStatement {
    Declaration(NameID, usize),
    AssignBool(ElaboratedAccessor, BoolExpression),
    AssignByte(ElaboratedAccessor, ByteExpression),
    AssignShort(ElaboratedAccessor, ShortExpression),
    AssignString(ElaboratedAccessor, StringExpression),
    Increment(ElaboratedAccessor, u8),
    LeftShift(ElaboratedAccessor, u32),
    RightShift(ElaboratedAccessor, u32),
    Clear(ElaboratedAccessor, usize),
    WriteBool(BoolExpression),
    WriteByte(ByteExpression),
    WriteShortAsNum(ShortExpression),
    WriteByteSequence(ElaboratedAccessor, usize), // add write string?
    WriteByteAsNum(ByteExpression),
    WriteConstString(Rc<str>),
    ReadByte(ElaboratedAccessor),
    While(BoolExpression, ElaboratedBlock),
    If(BoolExpression, ElaboratedBlock, Option<ElaboratedBlock>),
    Switch(ByteExpression, Box<[(u8, ElaboratedBlock)]>, Option<ElaboratedBlock>),
    // need to make this horrible mess better
    StackPush(NameID, usize),
    StackPop(NameID, usize)
}

pub type ElaboratedBlock = Vec<ElaboratedStatement>;

struct Elaborator {
    user_definitions: UserDefinitions,
    name_table: HashMap<Name, (NameID, ElaboratedDataType)>,
    current_block: ElaboratedBlock,
    next_name_id: NameID
}

impl Elaborator {

    fn fold_expression(&self, expression: &mut Expression) {

        replace(expression, match *expression {
            Expression::Add(box Expression::NumberLiteral(a), box Expression::NumberLiteral(b)) =>
                Expression::NumberLiteral(a + b),
            Expression::Subtract(box Expression::NumberLiteral(a), box Expression::NumberLiteral(b)) =>
                    Expression::NumberLiteral(a - b),
            Expression::Multiply(box Expression::NumberLiteral(a), box Expression::NumberLiteral(b)) =>
                Expression::NumberLiteral(a * b),
            Expression::Divide(box Expression::NumberLiteral(a), box Expression::NumberLiteral(b)) =>
                Expression::NumberLiteral(a / b),
            _ => return
        });
    }

    fn get_type_size(&self, data_type: &ElaboratedDataType) -> usize {
        data_type.get_size(&self.user_definitions)
    }

    fn get_field_data(&self, data_type: &ElaboratedDataType, field_name: Name) -> Option<(ElaboratedDataType, usize)> {
        Some(match (data_type, field_name.as_ref()) {
            (ElaboratedDataType::Short, "lower") => (ElaboratedDataType::Byte, 0),
            (ElaboratedDataType::Short, "upper") => (ElaboratedDataType::Byte, 1),
            (ElaboratedDataType::UserStruct(struct_id), _) => return self.user_definitions.structs[*struct_id].fields.get(&field_name).cloned(),
            _ => return None
        })
    }

    fn get_enum_variant_value(&self, enum_name: Name, variant_name: Name) -> Result<u8, BrainFricError> {

        let Some(&enum_id) = self.user_definitions.enum_name_table.get(&enum_name)
        else {
            err!(ElaborateError::UnknownEnum(enum_name));
        };
    
        let Some(&variant_value) = self.user_definitions.enums[enum_id].0.get(&variant_name)
        else {
            err!(ElaborateError::UnknownEnumVariant(enum_name, variant_name));
        };

        Ok(variant_value)
        
    }

    // bad code duplication 
    fn get_accessor_type(&self, accessor: &Accessor) -> Result<ElaboratedDataType, BrainFricError> {

        let mut data_type = self.name_table.get(accessor.name.as_ref()).map_or_else(
            ||
                err!(ElaborateError::UnknownIdentifier(accessor.name.clone())),
            |(_, data_type)|
                Ok(data_type.clone())
        )?;

        for specifier in accessor.specifiers.iter() {
            
            match (data_type, specifier) {
                (ElaboratedDataType::Sequence(inner_type, len), Specifier::Index(Expression::NumberLiteral(idx))) => {

                    if *idx as usize >= len {
                        err!(ElaborateError::OutOfBoundsAccess);
                    }

                    data_type = *inner_type;
                    
                }
                (data_type_again, Specifier::Field(field_name)) => {

                    let Some((new_data_type, field_offset)) = self.get_field_data(&data_type_again, field_name.clone())
                    else {
                        todo!("bad access");
                    };

                    data_type = new_data_type;
                
                }
                (ElaboratedDataType::Stack(inner_type, _), Specifier::StackPop) => {
                    data_type = *inner_type;
                }
                _ => todo!("bad access")
            }
        }

        Ok(data_type)

    }

    fn convert_accessor(&mut self, accessor: Accessor) -> Result<(ElaboratedDataType, ElaboratedAccessor), BrainFricError> {

        let (&name_id, mut data_type) = self.name_table.get(accessor.name.as_ref()).map_or_else(
            ||
                err!(ElaborateError::UnknownIdentifier(accessor.name.clone())),
            |(id, data_type)|
                Ok((id, data_type.clone()))
        )?;

        let mut specifiers = Vec::new();

        for specifier in accessor.specifiers.iter() {
            
            match (data_type, specifier) {
                (ElaboratedDataType::Sequence(inner_type, len), Specifier::Index(Expression::NumberLiteral(idx))) => {

                    if *idx as usize >= len {
                        err!(ElaborateError::OutOfBoundsAccess);
                    }

                    specifiers.push(ElaboratedSpecifier::ConstOffset(*idx as usize * self.get_type_size(&inner_type)));
                    data_type = *inner_type;
                    
                }
                (data_type_again, Specifier::Field(field_name)) => {

                    let Some((new_data_type, field_offset)) = self.get_field_data(&data_type_again, field_name.clone())
                    else {
                        todo!("bad access");
                    };
                    

                    specifiers.push(ElaboratedSpecifier::ConstOffset(field_offset));
                    data_type = new_data_type;

                }
                (ElaboratedDataType::Stack(inner_type, _), Specifier::StackPop) => {

                    self.current_block.push(ElaboratedStatement::StackPop(self.name_table[&accessor.name].0, self.get_type_size(&inner_type)));

                    specifiers.push(ElaboratedSpecifier::ConstOffset(1));

                    data_type = *inner_type;
                    
                }
                _ => todo!("bad access")
            }
        }

        let elaborated_accessor = ElaboratedAccessor {
            name_id,
            specifiers
        };

        Ok((data_type, elaborated_accessor))

    }

    fn get_expression_type(&self, expression: &Expression) -> Result<ElaboratedDataType, BrainFricError> {
        Ok(match expression {
            Expression::Access(accessor) => self.get_accessor_type(&accessor)?,
            Expression::Add(expr1, expr2) | 
            Expression::Subtract(expr1, expr2) | 
            Expression::Multiply(expr1, expr2) | 
            Expression::Divide(expr1, expr2) => {

                let type1 = self.get_expression_type(expr1)?;
                let type2 = self.get_expression_type(expr2)?;

                // probably not completely correct
                if type1 == ElaboratedDataType::Byte {
                    ElaboratedDataType::Byte
                }
                else if type1 == ElaboratedDataType::Short {
                    ElaboratedDataType::Short
                }
                else {
                    type2
                }
            }
            Expression::AsBool(_) | Expression::BoolLiteral(_) | Expression::And(_, _) | Expression::Or(_, _) | Expression::Not(_) | Expression::Equals(_, _) | Expression::NotEquals(_, _) | Expression::GreaterThan(_, _) | Expression::LessThan(_, _) | Expression::GreaterThanEqual(_, _) | Expression::LessThanEqual(_, _)
                => ElaboratedDataType::Bool,
                Expression::AsByte(_) => ElaboratedDataType::Byte,
            Expression::NumberLiteral(_) => ElaboratedDataType::GenericNumber,
            Expression::StringLiteral(_) => ElaboratedDataType::GenericString,
            Expression::EnumVariant(enum_name, _) => ElaboratedDataType::UserEnum(self.user_definitions.enum_name_table[enum_name])
        })
    }

    fn try_conflate_to_number(&self, expression1: &Expression, expression2: &Expression) -> Result<Option<ElaboratedDataType>, BrainFricError> {

        let expr1_type = self.get_expression_type(&expression1)?;
        let expr2_type = self.get_expression_type(&expression2)?;

        for data_type in [ElaboratedDataType::Byte, ElaboratedDataType::Short, ElaboratedDataType::GenericNumber] {
            if data_type.can_conflate(&expr1_type) && data_type.can_conflate(&expr2_type) {
                return Ok(Some(data_type));
            }
        }

        Ok(None)

    }

    fn try_conflate_to_equateable(&self, expression1: &Expression, expression2: &Expression) -> Result<Option<ElaboratedDataType>, BrainFricError> {

        let expr1_type = self.get_expression_type(&expression1)?;
        let expr2_type = self.get_expression_type(&expression2)?;

        Ok(self.try_conflate_to_number(expression1, expression2)?.or_else(|| {
            if let (ElaboratedDataType::UserEnum(enum1_id), ElaboratedDataType::UserEnum(enum2_id)) = (expr1_type, expr2_type) && enum1_id == enum2_id {
                Some(ElaboratedDataType::UserEnum(enum1_id))
            }
            else {
                None
            }
        }))
    }

    fn elaborate_bool_expression(&mut self, expression: Expression) -> Result<Box<BoolExpression>, BrainFricError> {

        Ok(Box::new(match expression {
            Expression::Access(parsed_accessor) => {

                let (data_type, accessor) = self.convert_accessor(parsed_accessor)?;
                self.assert_data_type(ElaboratedDataType::Bool, &data_type)?;

                BoolExpression::Access(accessor)

            }
            Expression::BoolLiteral(bool_value) => {
                BoolExpression::Constant(bool_value)
            }
            Expression::AsBool(expression) => {
                
                let data_type = self.get_expression_type(&expression)?;
                
                match data_type {
                    ElaboratedDataType::Byte => BoolExpression::ConvertByte(self.elaborate_byte_expression(*expression)?),
                    ElaboratedDataType::Short => BoolExpression::ConvertShort(self.elaborate_short_expression(*expression)?),
                    _ => todo!()
                }
            }
            Expression::And(expr1, expr2) =>
                BoolExpression::And(
                    self.elaborate_bool_expression(*expr1)?,
                    self.elaborate_bool_expression(*expr2)?
            ),
            Expression::Or(expr1, expr2) =>
                BoolExpression::Or(
                    self.elaborate_bool_expression(*expr1)?,
                    self.elaborate_bool_expression(*expr2)?
            ),
            Expression::Not(parsed_expression) =>
                BoolExpression::Not(self.elaborate_bool_expression(*parsed_expression)?
            ),
            Expression::Equals(expr1, expr2)
            => {

                match self.try_conflate_to_equateable(&expr1, &expr2)? {
                    Some(ElaboratedDataType::Byte | ElaboratedDataType::GenericNumber) => BoolExpression::ByteEquals(
                        self.elaborate_byte_expression(*expr1)?,
                        self.elaborate_byte_expression(*expr2)?
                    ),
                    Some(ElaboratedDataType::Short) => BoolExpression::ShortEquals(
                        self.elaborate_short_expression(*expr1)?,
                        self.elaborate_short_expression(*expr2)?
                    ),
                    Some(ElaboratedDataType::UserEnum(enum_id)) => BoolExpression::ByteEquals(
                        todo!(),
                        todo!()
                    ),
                    _ => todo!()//err!(self.current_todo!())
                }
            }
            Expression::NotEquals(expr1, expr2) => {
                
                match self.try_conflate_to_equateable(&expr1, &expr2)? {
                    Some(ElaboratedDataType::Byte | ElaboratedDataType::GenericNumber) => BoolExpression::ByteNotEquals(
                        self.elaborate_byte_expression(*expr1)?,
                        self.elaborate_byte_expression(*expr2)?
                    ),
                    Some(ElaboratedDataType::Short) => BoolExpression::ShortNotEquals(
                        self.elaborate_short_expression(*expr1)?,
                        self.elaborate_short_expression(*expr2)?
                    ),
                    Some(ElaboratedDataType::UserEnum(enum_id)) => BoolExpression::ByteNotEquals(
                        todo!(),
                        todo!()
                    ),
                    _ => {
                        println!("{expr1:?} {expr2:?} {:?}", self.try_conflate_to_equateable(&expr1, &expr2)?);
                        todo!()
                    }
                    //_ => todo!()//err!(self.current_todo!())
                }
            }
            Expression::GreaterThan(expr1, expr2) => {
                
                match self.try_conflate_to_number(&expr1, &expr2)? {
                    Some(ElaboratedDataType::Byte | ElaboratedDataType::GenericNumber) => BoolExpression::ByteGreaterThan(
                        self.elaborate_byte_expression(*expr1)?,
                        self.elaborate_byte_expression(*expr2)?
                    ),
                    Some(ElaboratedDataType::Short) => BoolExpression::ShortGreaterThan(
                        self.elaborate_short_expression(*expr1)?,
                        self.elaborate_short_expression(*expr2)?
                    ),
                    _ => todo!()//err!(self.current_todo!())
                }
            }
            Expression::GreaterThanEqual(expr1, expr2) => {
                
                match self.try_conflate_to_number(&expr1, &expr2)? {
                    Some(ElaboratedDataType::Byte | ElaboratedDataType::GenericNumber) => BoolExpression::ByteGreaterThanEqual(
                        self.elaborate_byte_expression(*expr1)?,
                        self.elaborate_byte_expression(*expr2)?
                    ),
                    Some(ElaboratedDataType::Short) => BoolExpression::ShortGreaterThanEqual(
                        self.elaborate_short_expression(*expr1)?,
                        self.elaborate_short_expression(*expr2)?
                    ),
                    _ => todo!()//err!(self.current_todo!())
                }
            }
            Expression::LessThan(expr1, expr2) => {
                
                match self.try_conflate_to_number(&expr1, &expr2)? {
                    Some(ElaboratedDataType::Byte | ElaboratedDataType::GenericNumber) => BoolExpression::ByteLessThan(
                        self.elaborate_byte_expression(*expr1)?,
                        self.elaborate_byte_expression(*expr2)?
                    ),
                    Some(ElaboratedDataType::Short) => BoolExpression::ShortLessThan(
                        self.elaborate_short_expression(*expr1)?,
                        self.elaborate_short_expression(*expr2)?
                    ),
                    _ => todo!()//err!(self.current_todo!())
                }
            }
            Expression::LessThanEqual(expr1, expr2) => {
                
                match self.try_conflate_to_number(&expr1, &expr2)? {
                    Some(ElaboratedDataType::Byte | ElaboratedDataType::GenericNumber) => BoolExpression::ByteLessThanEqual(
                        self.elaborate_byte_expression(*expr1)?,
                        self.elaborate_byte_expression(*expr2)?
                    ),
                    Some(ElaboratedDataType::Short) => BoolExpression::ShortLessThanEqual(
                        self.elaborate_short_expression(*expr1)?,
                        self.elaborate_short_expression(*expr2)?
                    ),
                    _ => todo!()//err!(self.current_todo!())
                }
            }
            _ => err!(ElaborateError::ExpectedTypedExpression(ElaboratedDataType::Bool))
        }))
    }

    fn elaborate_byte_expression(&mut self, expression: Expression) -> Result<Box<ByteExpression>, BrainFricError> {

        Ok(Box::new(match expression {
            Expression::Access(parsed_accessor) => {

                let (data_type, accessor) = self.convert_accessor(parsed_accessor)?;
                self.assert_data_type(ElaboratedDataType::Byte, &data_type)?;

                ByteExpression::Access(accessor)

            }
            Expression::NumberLiteral(number_value) => {
                ByteExpression::Constant(number_value as u8)
            }
            Expression::Add(expr1, expr2)
                => ByteExpression::Add(
                    self.elaborate_byte_expression(*expr1)?,
                    self.elaborate_byte_expression(*expr2)?
            ),
            Expression::Subtract(expr1, expr2)
                => ByteExpression::Subtract(
                    self.elaborate_byte_expression(*expr1)?,
                    self.elaborate_byte_expression(*expr2)?
            ),
            Expression::Multiply(expr1, expr2)
                => ByteExpression::Multiply(
                    self.elaborate_byte_expression(*expr1)?,
                    self.elaborate_byte_expression(*expr2)?
            ),
            Expression::Divide(expr1, expr2)
                => ByteExpression::Divide(
                    self.elaborate_byte_expression(*expr1)?,
                    self.elaborate_byte_expression(*expr2)?
            ),
            _ => err!(ElaborateError::ExpectedTypedExpression(ElaboratedDataType::Byte))
        }))
    }

    fn elaborate_short_expression(&mut self, expression: Expression) -> Result<Box<ShortExpression>, BrainFricError> {

        Ok(Box::new(match expression {
            Expression::Access(parsed_accessor) => {

                let (data_type, accessor) = self.convert_accessor(parsed_accessor)?;
                self.assert_data_type(ElaboratedDataType::Short, &data_type)?;

                ShortExpression::Access(accessor)

            }
            Expression::NumberLiteral(number_value) => {
                ShortExpression::Constant(number_value as u16)
            }
            Expression::Add(expr1, expr2)
                => ShortExpression::Add(
                    self.elaborate_short_expression(*expr1)?,
                    self.elaborate_short_expression(*expr2)?
            ),
            Expression::Subtract(expr2, expr1)
                => ShortExpression::Subtract(
                    self.elaborate_short_expression(*expr2)?,
                    self.elaborate_short_expression(*expr1)?
            ),
            Expression::Multiply(expr1, expr2)
                => ShortExpression::Multiply(
                    self.elaborate_short_expression(*expr1)?,
                    self.elaborate_short_expression(*expr2)?
            ),
            Expression::Divide(expr1, expr2)
                => ShortExpression::Divide(
                    self.elaborate_short_expression(*expr1)?,
                    self.elaborate_short_expression(*expr2)?
            ),
            _ => err!(ElaborateError::ExpectedTypedExpression(ElaboratedDataType::Byte))
        }))
    }

    fn elaborate_enum_expression(&mut self, expression: Expression, enum_id: usize) -> Result<Box<ByteExpression>, BrainFricError> {

        Ok(Box::new(match expression {
            Expression::EnumVariant(enum_name, variant_name)
                => ByteExpression::Constant(self.get_enum_variant_value(enum_name, variant_name)?),
            _ => err!(ElaborateError::ExpectedTypedExpression(ElaboratedDataType::UserEnum(enum_id))) // bad!
        }))
    }

    fn elaborate_string_expression(&mut self, expression: Expression, max_size: usize) -> Result<StringExpression, BrainFricError> {
        Ok(match expression {
            Expression::StringLiteral(string) => {

                if string.len() > max_size {
                    err!(ElaborateError::StringLiteralTooLarge(string, max_size))
                }

                StringExpression::Constant(string)
                
            }
            _ => err!(ElaborateError::ExpectedTypedExpression(ElaboratedDataType::GenericString))
        })
    }

    fn try_match_types(&self, type1: ElaboratedDataType, type2: ElaboratedDataType) -> Result<ElaboratedDataType, BrainFricError> {

        if type1 == type2 {
            Ok(type1)
        }
        else if type1.can_conflate(&type2) {
            Ok(type2)
        }
        else if type2.can_conflate(&type1) {
            Ok(type1)
        }
        else {
            err!(ElaborateError::TypeMismatch(type1, type2))
        }
    }

    fn assert_data_type(&self, expected_type: ElaboratedDataType, got_type: &ElaboratedDataType) -> Result<(), BrainFricError> {

        if expected_type != *got_type {
            err!(ElaborateError::TypeMismatch(expected_type.clone(), got_type.clone()))
        }

        Ok(())

    }

    fn assign(&mut self, mut accessor: ElaboratedAccessor, data_type: ElaboratedDataType, expression: Expression) -> Result<(), BrainFricError> {

        let statement = match data_type {
            ElaboratedDataType::Bool => 
                ElaboratedStatement::AssignBool(accessor, *self.elaborate_bool_expression(expression)?),
            ElaboratedDataType::Byte => 
                ElaboratedStatement::AssignByte(accessor, *self.elaborate_byte_expression(expression)?),
            ElaboratedDataType::Short => 
                ElaboratedStatement::AssignShort(accessor, *self.elaborate_short_expression(expression)?),
            ElaboratedDataType::UserEnum(enum_id) => 
                ElaboratedStatement::AssignByte(accessor, *self.elaborate_enum_expression(expression, enum_id)?),
            ElaboratedDataType::Sequence(box ElaboratedDataType::Byte, size) => 
                ElaboratedStatement::AssignString(accessor, self.elaborate_string_expression(expression, size)?),
            ElaboratedDataType::Stack(inner_type, _) => {

                let name_id = accessor.name_id;
                let size = self.get_type_size(&inner_type);

                accessor.specifiers.push(ElaboratedSpecifier::ConstOffset(1)); // dicey
                self.assign(accessor, *inner_type, expression)?;
                
                ElaboratedStatement::StackPush(name_id, size)

            }
            _ => todo!()
        };

        self.current_block.push(statement);

        Ok(())
    }

    fn elaborate_block(&mut self, block: Block) -> Result<ElaboratedBlock, BrainFricError> {

        let mut old_block = take(&mut self.current_block);

        for statement in block {

            set_line_num(statement.line_num);
        
            match statement.body {
                StatementBody::Declaration(names, parsed_data_type) => {

                    let data_type = ElaboratedDataType::convert_parsed(&parsed_data_type, &self.user_definitions)?;

                    let size = self.get_type_size(&data_type);

                    for name in names {

                        self.name_table.insert(name, (self.next_name_id, data_type.clone()));

                        self.current_block.push(ElaboratedStatement::Declaration(self.next_name_id, size));
                        self.next_name_id += 1;

                    }
                }
                StatementBody::Assign(parsed_accessor, mut expression) => {

                    let (accessor_data_type, accessor) = self.convert_accessor(parsed_accessor)?;

                    self.fold_expression(&mut expression);

                    // need to check if used accessor is in expression and copy if so

                    self.assign(accessor, accessor_data_type, expression)?;

                }
                StatementBody::Inc(accessor) => {

                    let (data_type, elaborated_accessor) = self.convert_accessor(accessor)?;
                    self.assert_data_type(ElaboratedDataType::Byte, &data_type)?;

                    self.current_block.push(ElaboratedStatement::Increment(elaborated_accessor, 1))

                }
                StatementBody::Dec(accessor) => {

                    let (data_type, elaborated_accessor) = self.convert_accessor(accessor)?;
                    self.assert_data_type(ElaboratedDataType::Byte, &data_type)?;
                    
                    self.current_block.push(ElaboratedStatement::Increment(elaborated_accessor, 255));

                }
                StatementBody::Clear(parsed_accessor) => {
                    
                    let (data_type, accessor) = self.convert_accessor(parsed_accessor)?;
                    
                    self.current_block.push(ElaboratedStatement::Clear(accessor, self.get_type_size(&data_type)));

                }
                StatementBody::Write(mut expression) => {

                    self.fold_expression(&mut expression);

                    let data_type = self.get_expression_type(&expression)?;

                    let statement = match data_type {
                        ElaboratedDataType::Bool => ElaboratedStatement::WriteBool(*self.elaborate_bool_expression(expression)?),
                        ElaboratedDataType::Byte => ElaboratedStatement::WriteByte(*self.elaborate_byte_expression(expression)?),
                        ElaboratedDataType::Sequence(box ElaboratedDataType::Byte, _) => ElaboratedStatement::WriteByteSequence(todo!(), todo!()),
                        _ => {

                            // make better at some point
                            if let Expression::StringLiteral(string) = expression {
                                ElaboratedStatement::WriteConstString(string)
                            }
                            else {
                                todo!()
                            }
                        }
                    };

                    self.current_block.push(statement);

                }
                StatementBody::WriteAsNum(mut parsed_expression) => {

                    self.fold_expression(&mut parsed_expression);

                    let data_type = self.get_expression_type(&parsed_expression)?;

                    let statement = match data_type {
                        ElaboratedDataType::Byte => ElaboratedStatement::WriteByteAsNum(*self.elaborate_byte_expression(parsed_expression)?),
                        ElaboratedDataType::Short => ElaboratedStatement::WriteShortAsNum(*self.elaborate_short_expression(parsed_expression)?),
                        _ => todo!()
                    };
                    
                    self.current_block.push(statement);

                }
                StatementBody::WriteLine => {
                    self.current_block.push(ElaboratedStatement::WriteByte(ByteExpression::Constant(10)))
                }
                StatementBody::Read(parsed_accessor) => {

                    let (data_type, accessor) = self.convert_accessor(parsed_accessor)?;
                    self.assert_data_type(ElaboratedDataType::Byte, &data_type)?;

                    self.current_block.push(ElaboratedStatement::ReadByte(accessor));
                
                }
                StatementBody::While(expression, block) => {

                    let expr = *self.elaborate_bool_expression(expression)?;
                    let loop_block = self.elaborate_block(block)?;

                    self.current_block.push(ElaboratedStatement::While(expr, loop_block));
                }
                StatementBody::If(expression, then_block, else_block) => {

                    let expr = *self.elaborate_bool_expression(expression)?;
                    let then = self.elaborate_block(then_block)?;
                    let r#else = if let Some(block) = else_block {
                        Some(self.elaborate_block(block)?)
                    }
                    else {
                        None
                    };

                    self.current_block.push(ElaboratedStatement::If(expr, then, r#else));
                }
                StatementBody::Switch(expression, parsed_arms, parsed_default_block) => {

                    let data_type = self.get_expression_type(&expression)?;

                    // fix when rust 0.80 comes out
                    let mut arms_vec = Vec::new();
                    for (arm, block) in Vec::from(parsed_arms).into_iter() {

                        arms_vec.push((
                            match arm {
                                MatchArm::EnumVariant(enum_name, variant_name) => self.get_enum_variant_value(enum_name, variant_name)?,
                                MatchArm::NumberLiteral(number) => number
                            },
                            self.elaborate_block(block)?)
                        );
                    }

                    let arms = arms_vec.into_boxed_slice();

                    let default_block = if let Some(block) = parsed_default_block {
                        Some(self.elaborate_block(block)?)
                    }
                    else {
                        None
                    };

                    let elaborated_expression = *match data_type {
                        ElaboratedDataType::Byte => self.elaborate_byte_expression(expression)?,
                        ElaboratedDataType::UserEnum(enum_id) => self.elaborate_enum_expression(expression, enum_id)?,
                        _ => todo!()
                    };

                    self.current_block.push(ElaboratedStatement::Switch(elaborated_expression, arms, default_block))

                }
                _ => todo!()
            }
        }

        swap(&mut self.current_block, &mut old_block);
        Ok(old_block)

    }
}

pub fn elaborate(parsed_program: ParsedProgram) -> Result<ElaboratedBlock, BrainFricError> {

    let mut elaborator = Elaborator {
        user_definitions: UserDefinitions::new(parsed_program.definitions)?,
        name_table: HashMap::new(),
        current_block: ElaboratedBlock::new(),
        next_name_id: 0
    };

    elaborator.elaborate_block(parsed_program.code)

}