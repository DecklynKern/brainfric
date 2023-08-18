use std::collections::HashMap;

use crate::parser::*;

struct Variable {
    data_type: DataType,
    address: usize
}

fn jump_to(variable: &Variable, data_head: &mut usize) -> String {

    let diff = variable.address as i32 - *data_head as i32;
    *data_head = variable.address;

    if diff > 0 {
        ">".repeat(diff as usize)

    } else {
        "<".repeat(-diff as usize)
    }
}

pub fn compile(statements: Vec<Statement>) -> String {
    
    let mut compiled = String::new();
    
    let mut variables: HashMap<String, Variable> = HashMap::new();
    let mut stack_pointer = 0;
    let mut data_head = 0;

    for statement in statements {

        match statement {
            Statement::Declaration(name, data_type) => {
                
                let size = data_type.get_size();

                variables.insert(name, Variable {
                    data_type,
                    address: stack_pointer
                });

                stack_pointer += size;

            }
            Statement::SetTo(name, value) => {

                if let Some(variable) = variables.get(&name) {

                    compiled.push_str(jump_to(variable, &mut data_head).as_str());

                    if let Expression::NumberLiteral(number) = value {
                        compiled.push_str("+".repeat(number).as_str());

                    } else {
                        todo!()
                    }

                } else {
                    todo!()
                }
            }
            Statement::Write(expression) => {

                if let Expression::Identifier(name) = expression {

                    if let Some(variable) = variables.get(&name) {
                        compiled.push_str(jump_to(variable, &mut data_head).as_str());
                        compiled.push('.');
                        
                    } else {
                        todo!()
                    }
                } else {
                    todo!()
                }
            }
            Statement::Read(expression) => {

                if let Expression::Identifier(name) = expression {

                    if let Some(variable) = variables.get(&name) {
                        compiled.push_str(jump_to(variable, &mut data_head).as_str());
                        compiled.push(',');
                        
                    } else {
                        todo!()
                    }
                } else {
                    todo!()
                }
            }
        }
    }

    compiled

}