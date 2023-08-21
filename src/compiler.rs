use std::collections::HashMap;

use crate::error::*;
use crate::parser::*;

struct Variable {
    data_type: DataType,
    address: usize,
    known_zeroed: bool
}

#[derive(Clone, Copy)]
enum Memory {
    Variable(usize),
    Register(usize),
    Address(usize)
}

pub struct Compiler {
    statements: Vec<Statement>,
    statements_parsed: usize,
    compiled: String,
    variables: Vec<Variable>,
    name_table: HashMap<String, usize>,
    stack_pointer: usize,
    data_head: usize
}

impl Compiler {

    pub fn new(mut statements: Vec<Statement>) -> Self {

        statements.reverse();

        Self {
            statements,
            statements_parsed: 0,
            compiled: String::new(),
            variables: Vec::new(),
            name_table: HashMap::new(),
            stack_pointer: 0,
            data_head: 0
        }
    }

    fn push_code(&mut self, code: &str) {
        self.compiled.push_str(code);
    }

    fn jump_to(&mut self, memory: Memory) {

        let address = match memory {
            Memory::Variable(idx) => self.variables[idx].address,
            Memory::Register(offset) => self.stack_pointer + offset,
            Memory::Address(address) => address
        };
    
        let diff = address as i32 - self.data_head as i32;
        self.data_head = address;

        self.push_code(if diff > 0 {
            ">".repeat(diff as usize)
    
        } else {
            "<".repeat(-diff as usize)

        }.as_str());

    }

    fn add_const(&mut self, memory: Memory, mut num: i32) {

        if let Memory::Variable(idx) = memory && self.variables[idx].data_type != DataType::Byte {
            todo!()
        }

        self.jump_to(memory);

        // slight optimization
        if num > 128 {
            num = num - 256;

        } else if num < -128 {
            num = num + 256;
        }

        self.push_code(if num > 0 {
            "+".repeat(num as usize)
    
        } else {
            "-".repeat(-num as usize)

        }.as_str());

    }

    fn sub_const(&mut self, memory: Memory, num: i32) {
        self.add_const(memory, -num);
    }

    fn clear(&mut self, memory: Memory) {

        match memory {
            Memory::Variable(idx) => {

                if self.variables[idx].known_zeroed {
                    return;
                }

                self.jump_to(memory.clone());

                for i in 0..self.variables[idx].data_type.get_size() {
                    self.push_code("[-]>");
                }

                self.compiled.pop();

                self.data_head += self.variables[idx].data_type.get_size() - 1;

            }
            Memory::Register(_) | Memory::Address(_) => {
                self.jump_to(memory.clone());
                self.push_code("[-]");
            }
        }
    }

    fn free_variable(&mut self, name: &String) {
        
        let idx = self.name_table.remove(name).unwrap();
        self.clear(Memory::Variable(idx));

        let variable = self.variables.remove(idx);
        
        if variable.address + variable.data_type.get_size() == self.stack_pointer {
            self.stack_pointer -= variable.data_type.get_size();
        }
    }

    fn has_use(&self, name: &String) -> bool {
        self.statements.iter().any(|statement| statement.uses_variable(name))
    }

    fn is_last_use(&self, name: &String) -> bool {
        self.statements.iter().rposition(|statement|
            statement.uses_variable(name)).is_some_and(|idx| idx == self.statements.len() - 1
        )
    }

    fn handle_declaration(&mut self, name: &String, data_type: DataType) -> Result<(), CompilerError> {

        // in theory we check usage but 
                    
        let size = data_type.get_size();

        self.name_table.insert(name.clone(), self.variables.len());

        self.variables.push(Variable {
            data_type: data_type.clone(),
            address: self.stack_pointer,
            known_zeroed: true
        });

        self.stack_pointer += size;

        Ok(())

    }

    fn handle_set_to(&mut self, name: &String, value: Expression) -> Result<(), CompilerError> {

        if let Some(&idx) = self.name_table.get(name) {

            let mem = Memory::Variable(idx);

            self.clear(mem);

            self.variables[idx].known_zeroed = false;

            if let Expression::NumberLiteral(number) = value {

                self.add_const(mem, number as i32);

                if number == 0 {
                    self.variables[idx].known_zeroed = true;
                }

                Ok(())

            } else {
                todo!()
            }

        } else {
            Err(CompilerError::UnknownIdentifier(name.clone()))
        }
    }

    fn handle_write(&mut self, expression: Expression) -> Result<(), CompilerError> {

        if let Expression::Identifier(name) = expression {
    
            if let Some(&idx) = self.name_table.get(&name) {
                self.jump_to(Memory::Variable(idx));
                self.push_code(".");                

            } else {
                todo!()
            }
            
        } else if let Expression::StringLiteral(val) = expression {

            let mem = Memory::Register(0);
            self.jump_to(mem);

            let mut last_char_val = 0;

            for char in val.chars() {

                let char_val = char as i32;
            
                if char_val < 256 {

                    self.add_const(mem, char_val - last_char_val);
                    self.push_code(".");

                    last_char_val = char_val;

                } else {
                    todo!()
                }
            }

            if !val.is_empty() {
                self.clear(mem);
            }
        
        } else {
            todo!()
        }

        Ok(())

    }

    fn handle_read(&mut self, name: &String) -> Result<(), CompilerError> {

        if let Some(&idx) = self.name_table.get(name) {
            self.jump_to(Memory::Variable(idx));
            self.push_code(",");

            Ok(())
            
        } else {
            Err(CompilerError::UnknownIdentifier(name.clone()))
        }
    }

    pub fn compile(&mut self) -> Result<String, CompilerError> {
    
        while let Some(statement) = self.statements.pop() {
    
            match statement {
                Statement::Declaration(name, data_type) => 
                    self.handle_declaration(&name, data_type)?,

                Statement::SetTo(name, value) => 
                    self.handle_set_to(&name, value)?,

                Statement::Write(expression) => 
                    self.handle_write(expression)?,

                Statement::Read(name) => 
                    self.handle_read(&name)?,
            }

            self.statements_parsed += 1;

        }

        let mut blank = String::new();
        std::mem::swap(&mut blank, &mut self.compiled);
        Ok(blank)

    }
}