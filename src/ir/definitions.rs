use std::collections::HashMap;

use crate::lex::Name;
use crate::parse::Definition;

pub type EnumID = usize;

pub struct UserDefinitions {
    pub enums: HashMap<Name, EnumID>,
    pub enum_variants: HashMap<(EnumID, Name), u8>
}

pub fn get_user_definitions(parsed_definitions: Vec<Definition>) -> UserDefinitions {

    let mut user_definitions = UserDefinitions {
        enums: HashMap::new(),
        enum_variants: HashMap::new()
    };

    let mut next_enum_id = 0;

    for parsed_definition in parsed_definitions {
        match parsed_definition {
            Definition::Enum(name, variants) => {

                user_definitions.enums.insert(name, next_enum_id);
                for (variant_name, value) in variants {
                    user_definitions.enum_variants.insert((next_enum_id, variant_name), value);
                }
                
                next_enum_id += 1;
                
            }
        }
    }

    user_definitions

}