use std::collections::HashMap;

use crate::lex::Name;
use crate::parse::*;
use crate::error::*;
use crate::err;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DataType {
    Bool,
    Byte,
    Short,
    Sequence(Box<DataType>, usize),
    String(usize),
    Stack(Box<DataType>, usize),
    UserEnum(usize),
    UserStruct(usize)
    //Array(Rc<DataType>, usize)
}

impl DataType {

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

    pub fn convert_parsed(parsed_data_type: &ParsedDataType, line_num: usize, user_definitions: &UserDefinitions) -> Result<Self, BrainFricError> {

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
            err!(line_num, IRError::InvalidTypeParameters);
        }

        for idx in 0..expected_types.len() {
            if expected_types[idx] != params[idx].get_type_val() {
                err!(line_num, IRError::InvalidTypeParameters);
            }
        }

        Ok(match &parsed_data_type.head {
            DataTypeHead::Bool => DataType::Bool,
            DataTypeHead::Byte => DataType::Byte,
            DataTypeHead::Short => DataType::Short,
            DataTypeHead::Sequence => DataType::Sequence(
                Box::new(Self::convert_parsed(Self::get_subtype(&params[0]), line_num, user_definitions)?),
                Self::get_constant_val(&params[1])
            ),
            DataTypeHead::String => DataType::String(
                Self::get_constant_val(&params[0])
            ),
            DataTypeHead::Stack => DataType::Stack(
                Box::new(Self::convert_parsed(Self::get_subtype(&params[0]), line_num, user_definitions)?),
                Self::get_constant_val(&params[1])
            ),
            DataTypeHead::UserDefined(name) => {

                match user_definitions.enum_name_table.get(name) {
                    Some(enum_id) => DataType::UserEnum(*enum_id),
                    None => match user_definitions.struct_name_table.get(name) {
                        Some(struct_id) => DataType::UserStruct(*struct_id),
                        None => err!(line_num, IRError::UnknownType(name.clone()))
                    }
                }
            }
        })
    }

    pub fn get_size(&self, user_definitions: &UserDefinitions) -> usize {

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
}

pub type Identifier = usize;

pub struct UserEnum(pub HashMap<Name, u8>);

pub struct UserStruct {
    pub size: usize,
    pub fields: HashMap<Name, (DataType, usize)>
}

pub struct UserDefinitions {
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
                        
                        let data_type = DataType::convert_parsed(&parsed_data_type, usize::MAX, &user_definitions)?;
                    
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