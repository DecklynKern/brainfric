use std::collections::HashMap;
use std::rc::Rc;

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
                        
                        let data_type = ElaboratedDataType::convert_parsed(&parsed_data_type, usize::MAX, &user_definitions)?;
                    
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
    Sequence(Box<ElaboratedDataType>, usize),
    String(usize),
    GenericNumber,
    GenericString,
    Stack(Box<ElaboratedDataType>, usize),
    UserEnum(usize),
    UserStruct(usize)
    //Array(Rc<DataType>, usize)
}

impl ElaboratedDataType {

    fn can_conflate(&self, other: &Self) -> bool {

        matches!(
            (self, other),
            (Self::GenericNumber, Self::Byte | Self::Short) |
            (Self::GenericString, Self::String(_) | Self::Sequence(box Self::Byte, _))
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
            Self::Sequence(data_type, len) => len * data_type.get_size(user_definitions),
            Self::String(len) => len + 2,
            Self::Stack(data_type, len) => len * (data_type.get_size(user_definitions) + 1) + 1,
            Self::UserEnum(_) => 1,
            Self::UserStruct(struct_id) => user_definitions.structs[*struct_id].size
            //Self::Array(data_type, len) => data_type.get_size() * len
        }
    }
    
    fn convert_parsed(parsed_data_type: &ParsedDataType, line_num: usize, user_definitions: &UserDefinitions) -> Result<Self, BrainFricError> {

        let params = &parsed_data_type.parameters;

        const CONSTANT: u32 = 0;
        const TYPE: u32 = 1;

        let expected_types = match parsed_data_type.head {
            DataTypeHead::Sequence => vec![TYPE, CONSTANT],
            DataTypeHead::String => vec![CONSTANT],
            DataTypeHead::Stack => vec![TYPE, CONSTANT],
            _ => vec![]
        };

        if expected_types.len() != params.len() {
            err!(line_num, ElaborateError::InvalidTypeParameters);
        }

        for idx in 0..expected_types.len() {
            if expected_types[idx] != params[idx].get_type_val() {
                err!(line_num, ElaborateError::InvalidTypeParameters);
            }
        }

        Ok(match &parsed_data_type.head {
            DataTypeHead::Bool => Self::Bool,
            DataTypeHead::Byte => Self::Byte,
            DataTypeHead::Short => Self::Short,
            DataTypeHead::Sequence => Self::Sequence(
                Box::new(Self::convert_parsed(Self::get_subtype(&params[0]), line_num, user_definitions)?),
                Self::get_constant_val(&params[1])
            ),
            DataTypeHead::String => Self::String(
                Self::get_constant_val(&params[0])
            ),
            DataTypeHead::Stack => Self::Stack(
                Box::new(Self::convert_parsed(Self::get_subtype(&params[0]), line_num, user_definitions)?),
                Self::get_constant_val(&params[1])
            ),
            DataTypeHead::UserDefined(name) => {

                match user_definitions.enum_name_table.get(name) {
                    Some(enum_id) => Self::UserEnum(*enum_id),
                    None => match user_definitions.struct_name_table.get(name) {
                        Some(struct_id) => Self::UserStruct(*struct_id),
                        None => err!(line_num, ElaborateError::UnknownType(name.clone()))
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
    pub specifiers: Box<[ElaboratedSpecifier]>
}

impl std::fmt::Debug for ElaboratedAccessor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Accessor(\"{}\", {:?})", self.name_id, self.specifiers)
    }
}

#[derive(Debug)]
pub enum BoolExpression {

    Access(ElaboratedAccessor),
    Constant(bool),

    ByteEquals(Box<ByteExpression>, Box<ByteExpression>),
    ByteNotEquals(Box<ByteExpression>, Box<ByteExpression>),
    ByteLessThan(Box<ByteExpression>, Box<ByteExpression>),
    ByteGreaterThan(Box<ByteExpression>, Box<ByteExpression>),
    ByteLessThanEqual(Box<ByteExpression>, Box<ByteExpression>),
    ByteGreaterThanEqual(Box<ByteExpression>, Box<ByteExpression>),

    Not(Box<BoolExpression>),
    And(Box<BoolExpression>, Box<BoolExpression>),
    Or(Box<BoolExpression>, Box<BoolExpression>),

    ConvertByte(Box<ByteExpression>)

}

#[derive(Debug)]
pub enum ByteExpression {

    Access(ElaboratedAccessor),

    Constant(u8),

    Add(Box<ElaboratedExpression>, Box<ElaboratedExpression>),
    Subtract(Box<ElaboratedExpression>, Box<ElaboratedExpression>),
    Multiply(Box<ElaboratedExpression>, Box<ElaboratedExpression>),
    Divide(Box<ElaboratedExpression>, Box<ElaboratedExpression>),

    ConvertBool(Box<BoolExpression>),
    ConvertShort(Box<ShortExpression>)

}

#[derive(Debug)]
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
    Access(ElaboratedAccessor),
    Constant(Rc<str>)
}

#[derive(Debug)]
pub enum ElaboratedExpression {
    Bool(BoolExpression),
    Byte(ByteExpression),
    Short(ShortExpression),
    String(StringExpression)
}

#[derive(Debug)]
pub enum ElaboratedStatement {
    Declaration(NameID, usize),
    Assign(ElaboratedAccessor, ElaboratedExpression),
    Increment(ElaboratedAccessor, u8),
    Clear(ElaboratedAccessor),
    LeftShift(ElaboratedAccessor, u32),
    RightShift(ElaboratedAccessor, u32),
    Write(Expression),
    WriteNum(Expression),
    Read(ElaboratedAccessor),
    While(Expression, Block),
    If(Expression, Block, Option<Block>),
    Switch(Expression, Vec<(MatchArm, Block)>, Option<Block>)
}

type ElaboratedBlock = Vec<ElaboratedStatement>;

struct Elaborator {
    user_definitions: UserDefinitions,
    name_table: HashMap<Name, (NameID, ElaboratedDataType)>,
    current_line_num: usize,
    next_name_id: NameID
}

impl Elaborator {

    fn get_field_data(&self, data_type: &ElaboratedDataType, field_name: Name) -> Option<(ElaboratedDataType, usize)> {
        Some(match (data_type, field_name.as_ref()) {
            (ElaboratedDataType::Short, "lower") => (ElaboratedDataType::Byte, 0),
            (ElaboratedDataType::Short, "upper") => (ElaboratedDataType::Byte, 1),
            (ElaboratedDataType::UserStruct(struct_id), _) => return self.user_definitions.structs[*struct_id].fields.get(&field_name).cloned(),
            _ => return None
        })
    }

    fn convert_accessor(&self, accessor: Accessor) -> Result<(ElaboratedDataType, ElaboratedAccessor), BrainFricError> {

        let (&name_id, mut data_type) = self.name_table.get(accessor.name.as_ref()).map_or_else(
            ||
                err!(self.current_line_num, ElaborateError::UnknownIdentifier(accessor.name.clone())),
            |(id, data_type)|
                Ok((id, data_type.clone()))
        )?;

        let mut offset = 0;

        for specifier in accessor.specifiers.iter() {
            
            match (data_type, specifier) {
                (ElaboratedDataType::Sequence(inner_type, len), Specifier::ConstIndex(idx)) => {

                    if *idx as usize >= len {
                        err!(self.current_line_num, ElaborateError::OutOfBoundsAccess);
                    }

                    offset += *idx as usize * inner_type.get_size(&self.user_definitions);
                    data_type = *inner_type;
                    
                }
                (ElaboratedDataType::String(len), Specifier::ConstIndex(idx)) => {

                    if *idx as usize >= len {
                        err!(self.current_line_num, ElaborateError::OutOfBoundsAccess);
                    }

                    offset += *idx as usize + 1;
                    data_type = ElaboratedDataType::Byte;
                    
                }
                (data_type_again, Specifier::Field(field_name)) => {

                    let Some((new_data_type, field_offset)) = self.get_field_data(&data_type_again, field_name.clone())
                    else {
                        todo!("bad access");
                    };
                    
                    offset += field_offset;
                    data_type = new_data_type;

                }
                _ => todo!("bad access")
            }
        }

        let specifiers = if offset != 0 {
            vec![ElaboratedSpecifier::ConstOffset(offset)]
        }
        else {
            Vec::new()
        }.into_boxed_slice();

        let elaborated_accessor = ElaboratedAccessor {
            name_id,
            specifiers
        };

        Ok((data_type, elaborated_accessor))

    }

    fn convert_bool_expression(&self, expression: Expression) -> Result<BoolExpression, BrainFricError> {

        match expression {
            Expression::And(parsed_expression1, parsed_expression2) => {

                let expr1 = self.convert_bool_expression(*parsed_expression1)?;
                let expr2 = self.convert_bool_expression(*parsed_expression2)?;

                (
                    ElaboratedDataType::Bool,
                    ElaboratedExpression::Bool(BoolExpression::And(
                        Box::new(expr1),
                        Box::new(expr2)
                    ))
                )
            }
        }
    }

    fn convert_byte_expression(&self, expression: Expression) -> Result<ByteExpression, BrainFricError> {

        match expression {
        }
    }

    fn convert_short_expression(&self, expression: Expression) -> Result<ShortExpression, BrainFricError> {

        match expression {
        }
    }

    fn convert_expression(&self, expression: Expression) -> Result<(ElaboratedDataType, ElaboratedExpression), BrainFricError> {

        Ok(match expression {

            /*

    Access(Accessor),
    EnumVariant(Name, Name),
    BoolLiteral(bool),
    NumberLiteral(i32),
    StringLiteral(Rc<str>),

    Equals(Box<Expression>, Box<Expression>),
    NotEquals(Box<Expression>, Box<Expression>),
    LessThan(Box<Expression>, Box<Expression>),
    GreaterThan(Box<Expression>, Box<Expression>),
    LessThanEqual(Box<Expression>, Box<Expression>),
    GreaterThanEqual(Box<Expression>, Box<Expression>),

    Not(Box<Expression>),
    And(Box<Expression>, Box<Expression>),
    Or(Box<Expression>, Box<Expression>),

    Add(Box<Expression>, Box<Expression>),
    Subtract(Box<Expression>, Box<Expression>),
    Multiply(Box<Expression>, Box<Expression>),
    Divide(Box<Expression>, Box<Expression>),

    AsBool(Box<Expression>),
    AsNum(Box<Expression>) */

            Expression::Access(parsed_accessor) => {

                let (accessor_data_type, accessor) = self.convert_accessor(parsed_accessor)?;

                let expression = match accessor_data_type {
                    ElaboratedDataType::Bool =>
                        ElaboratedExpression::Bool(BoolExpression::Access(accessor)),
                    ElaboratedDataType::Byte | 
                    ElaboratedDataType::UserEnum(_) =>
                        ElaboratedExpression::Byte(ByteExpression::Access(accessor)),
                    ElaboratedDataType::Short =>
                        ElaboratedExpression::Short(ShortExpression::Access(accessor)),
                    _ => todo!()
                };

                (accessor_data_type, expression)

            }
            
            Expression::EnumVariant(enum_name, variant_name) => {
                
                let enum_id = self.user_definitions.enum_name_table[&enum_name];
                let variant_value = self.user_definitions.enums[enum_id].0[&variant_name];

                (
                    ElaboratedDataType::UserEnum(enum_id),
                    ElaboratedExpression::Byte(ByteExpression::Constant(variant_value))
                )
            }
            Expression::BoolLiteral(bool_value) => (
                ElaboratedDataType::Bool,
                ElaboratedExpression::Bool(BoolExpression::Constant(bool_value))
            ),
            Expression::NumberLiteral(number_value) => todo!("ambiguous"),
            Expression::StringLiteral(string_value) => todo!("ambiguous size"),

            Expression::Add(parsed_expression1, parsed_expression2) => {

                let (expr1_type, expr1) = self.convert_expression(*parsed_expression1)?;
                let (expr2_type, expr2) = self.convert_expression(*parsed_expression2)?;

                let combined_type = self.try_match_types(expr1_type, expr2_type)?;

                (combined_type, )

            }

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
            err!(self.current_line_num, ElaborateError::TypeMismatch(type1, type2))
        }
    }

    fn assert_data_type(&self, expected_type: ElaboratedDataType, got_type: &ElaboratedDataType) -> Result<(), BrainFricError> {

        if *expected_type != *got_type {
            err!(self.current_line_num, ElaborateError::TypeMismatch(expected_type.clone(), got_type.clone()))
        }

        Ok(())

    }

    pub fn elaborate_block(&mut self, block: Block) -> Result<ElaboratedBlock, BrainFricError> {

        let mut elaborated_block = ElaboratedBlock::new();

        for statement in block {

            self.current_line_num = statement.line_num;
        
            match statement.body {
                StatementBody::Declaration(names, parsed_data_type) => {

                    let data_type = ElaboratedDataType::convert_parsed(&parsed_data_type, statement.line_num, &self.user_definitions)?;

                    let size = data_type.get_size(&self.user_definitions);

                    for name in names {

                        self.name_table.insert(name, (self.next_name_id, data_type.clone()));

                        elaborated_block.push(ElaboratedStatement::Declaration(self.next_name_id, size));
                        self.next_name_id += 1;

                    }
                }
                StatementBody::Assign(parsed_accessor, parsed_expression) => {

                    let (accessor_data_type, accessor) = self.convert_accessor(parsed_accessor)?;

                    let converted_expression = match accessor_data_type {
                        ElaboratedDataType::Bool => ElaboratedExpression::Bool(self.convert_bool_expression(parsed_expression)?),
                        ElaboratedDataType::Byte => ElaboratedExpression::Byte(self.convert_byte_expression(parsed_expression)?),
                        ElaboratedDataType::Short => ElaboratedExpression::Short(self.convert_short_expression(parsed_expression)?),
                        _ => todo!()
                    };

                    elaborated_block.push(ElaboratedStatement::Assign(accessor, expression));

                }
                StatementBody::Inc(accessor) => {

                    let (data_type, elaborated_accessor) = self.convert_accessor(accessor)?;
                    self.assert_data_type(&data_type, &ElaboratedDataType::Byte)?;

                    elaborated_block.push(ElaboratedStatement::Increment(elaborated_accessor, 1))

                }
                StatementBody::Dec(accessor) => {

                    let (data_type, elaborated_accessor) = self.convert_accessor(accessor)?;
                    self.assert_data_type(&data_type, &ElaboratedDataType::Byte)?;
                    
                    elaborated_block.push(ElaboratedStatement::Increment(elaborated_accessor, 255));

                }
                _ => todo!()
            }
        }

        Ok(elaborated_block)

    }
}

pub fn elaborate(parsed_program: ParsedProgram) -> Result<ElaboratedBlock, BrainFricError> {

    let mut elaborator = Elaborator {
        user_definitions: UserDefinitions::new(parsed_program.definitions)?,
        name_table: HashMap::new(),
        current_line_num: 0,
        next_name_id: 0
    };

    elaborator.elaborate_block(parsed_program.code)

}