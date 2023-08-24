use std::collections::HashMap;

use crate::error::*;
use crate::err;
use crate::parse::*;

type VarIdx = usize;
type Error = Result<(), BrainFricError>;

struct Variable {
    data_type: DataType,
    address: usize,
    known_zeroed: bool
}

#[derive(Clone, Copy)]
enum Memory {
    Variable(VarIdx),
    Absolute(usize)
}

pub struct Compiler {
    statements: Vec<(usize, Statement)>,
    current_line_num: usize,
    compiled: String,
    variables: Vec<Variable>,
    name_table: HashMap<Name, VarIdx>,
    stack_pointer: usize,
    next_register_offset: usize,
    data_head: usize
}

impl Compiler {

    pub fn new(mut statements: Vec<(usize, Statement)>) -> Self {

        statements.reverse();

        Self {
            statements,
            current_line_num: 0,
            compiled: String::new(),
            variables: Vec::new(),
            name_table: HashMap::new(),
            stack_pointer: 0,
            next_register_offset: 0,
            data_head: 0
        }
    }

    pub fn get_expression_data_type(&self, expression: &Expression) -> Result<DataType, BrainFricError> {

        Ok(match expression {
            Expression::BoolLiteral(_) => DataType::Bool,
            Expression::NumberLiteral(_) => DataType::Byte, // not correct :(
            Expression::StringLiteral(_) => todo!(), // strings lol
            Expression::Equals(expr1, expr2) |
            Expression::GreaterThan(expr1, expr2) |
            Expression::LessThan(expr1, expr2) |
            Expression::And(expr1, expr2) |
            Expression::Or(expr1, expr2) => {
                self.assert_expression_data_type(expr1, DataType::Bool)?;
                self.assert_expression_data_type(expr2, DataType::Bool)?;
                DataType::Bool
            }
            Expression::Not(expr) => {
                self.assert_expression_data_type(expr, DataType::Bool)?;
                DataType::Bool
            },
            Expression::Add(expr1, expr2) |
            Expression::Subtract(expr1, expr2) => {
                // also not quite correct
                self.assert_expression_data_type(expr1, DataType::Byte)?;
                self.assert_expression_data_type(expr2, DataType::Byte)?;
                DataType::Byte
            }
            Expression::Identifier(name) => {
                self.variables[self.get_variable_idx(name)?].data_type.clone()
            }
        })
    }

    fn assert_expression_data_type(&self, expression: &Expression, data_type: DataType) -> Error {

        let expression_type = self.get_expression_data_type(expression)?;

        if expression_type != data_type {
            err!(self.current_line_num, CompileError::IncorrectExpressionType(data_type, expression_type))
        }

        Ok(())

    }

    fn assert_variable_data_type(&self, name: &Name, data_type: DataType) -> Error {

        let variable_type = &self.variables[self.get_variable_idx(name)?].data_type;

        if *variable_type != data_type {
            err!(self.current_line_num, CompileError::IncorrectExpressionType(data_type, variable_type.clone()))
        }

        Ok(())

    }

    fn assert_equal_type(&self, name: &Name, expression: &Expression) -> Result<&DataType, BrainFricError> {

        let var_type = &self.variables[self.get_variable_idx(name)?].data_type;
        let expr_type = self.get_expression_data_type(expression)?;

        if *var_type != expr_type {
            err!(self.current_line_num, CompileError::TypeMismatch(var_type.clone(), expr_type))
        }

        Ok(var_type)

    }

    fn push_code(&mut self, code: &str) {
        self.compiled.push_str(code);
    }

    fn allocate_register(&mut self) -> Memory {
        let mem = Memory::Absolute(self.stack_pointer + self.next_register_offset);
        self.next_register_offset += 1;
        mem
    }

    fn free_register(&mut self, register: Memory) -> Error {

        if let Memory::Absolute(address) = register && address + 1 == self.stack_pointer + self.next_register_offset {
            // it better have been cleared
            self.next_register_offset -= 1;
            Ok(())  

        } else if let Memory::Variable(_) = register {
            err!(self.current_line_num, InternalCompilerError::FreeVariableAsRegister);

        } else {
            err!(self.current_line_num, InternalCompilerError::BadRegisterFreeOrder);
        }
    }

    fn get_variable_idx(&self, name: &Name) -> Result<VarIdx, BrainFricError> {

        if let Some(idx) = self.name_table.get(name) {
            Ok(*idx)

        } else {
            err!(self.current_line_num, CompileError::UnknownIdentifier(name.to_string()))
        }
    }

    fn free_variable(&mut self, name: &Name) {
        
        let idx = self.name_table.remove(name).unwrap();
        self.clear(Memory::Variable(idx));

        let variable = self.variables.remove(idx);
        
        if variable.address + variable.data_type.get_size() == self.stack_pointer {
            self.stack_pointer -= variable.data_type.get_size();
        }
    }

    fn has_use(&self, name: &Name) -> bool {
        self.statements.iter().any(|(_, statement)| statement.uses_variable(name))
    }

    fn is_last_use(&self, name: &Name) -> bool {
        self.statements.iter().rposition(|(_, statement)|
            statement.uses_variable(name)).is_some_and(|idx| idx == self.statements.len() - 1
        )
    }

    fn jump_to(&mut self, memory: Memory) {

        let address = match memory {
            Memory::Variable(idx) => self.variables[idx].address,
            Memory::Absolute(address) => address
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

                for _ in 0..self.variables[idx].data_type.get_size() {
                    self.push_code("[-]>");
                }

                self.compiled.pop();

                self.data_head += self.variables[idx].data_type.get_size() - 1;

            }
            Memory::Absolute(_) => {
                self.jump_to(memory.clone());
                self.push_code("[-]");
            }
        }
    }

    fn move_byte(&mut self, mems_to: &[Memory], mem_from: Memory) {

        self.jump_to(mem_from);
        self.push_code("[-");

        for mem_to in mems_to {
            self.jump_to(*mem_to);
            self.push_code("+");
        }

        self.jump_to(mem_from);
        self.push_code("]");

    }

    fn copy_byte(&mut self, mems_to: &[Memory], mem_from: Memory) -> Error {
        
        let reg = self.allocate_register();
        let mut copy_back = mems_to.to_vec();
        copy_back.push(mem_from);

        self.move_byte(&[reg], mem_from);
        self.move_byte(copy_back.as_slice(), reg);

        self.free_register(reg)?;
        Ok(())

    }

    fn handle_declaration(&mut self, name: &Name, data_type: DataType) -> Error {

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

    fn handle_set_to(&mut self, name: &Name, value: Expression) -> Error {

        self.assert_equal_type(name, &value)?;

        let idx = self.get_variable_idx(name)?;
        let mem = Memory::Variable(idx);

        self.clear(mem);

        self.variables[idx].known_zeroed = false;

        if let Expression::NumberLiteral(number) = value {

            self.add_const(mem, number as i32);

            if number == 0 {
                self.variables[idx].known_zeroed = true;
            }

            Ok(())

        } else if let Expression::Identifier(name_from) = value {

            let idx_from = self.get_variable_idx(&name_from)?;
            self.copy_byte(&[mem], Memory::Variable(self.variables[idx_from].address))?;

            Ok(())

        } else {
            todo!()
        }
    }

    fn handle_inc(&mut self, name: &Name) -> Error {

        self.assert_variable_data_type(name, DataType::Byte)?;

        self.add_const(Memory::Variable(self.get_variable_idx(name)?), 1);
        Ok(())
        
    }

    fn handle_dec(&mut self, name: &Name) -> Error {

        self.assert_variable_data_type(name, DataType::Byte)?;

        self.sub_const(Memory::Variable(self.get_variable_idx(name)?), 1);
        Ok(())

    }

    fn handle_write(&mut self, expression: Expression) -> Error {

        if let Expression::Identifier(name) = expression {

            self.jump_to(Memory::Variable(self.get_variable_idx(&name)?));
            self.push_code(".");
            
        } else if let Expression::StringLiteral(val) = expression {

            let reg = self.allocate_register();
            self.jump_to(reg);

            let mut last_char_val = 0;

            for char in val.chars() {

                let char_val = char as i32;
            
                if char_val < 256 {

                    self.add_const(reg, char_val - last_char_val);
                    self.push_code(".");

                    last_char_val = char_val;

                } else {
                    err!(self.current_line_num, CompileError::NumberLiteralTooLarge(char as u32));
                }
            }

            if !val.is_empty() {
                self.clear(reg);
            }

            self.free_register(reg)?;
        
        } else {
            todo!()
        }

        Ok(())

    }

    fn handle_read(&mut self, name: &Name) -> Error {

        self.assert_variable_data_type(name, DataType::Byte)?;

        self.jump_to(Memory::Variable(self.get_variable_idx(name)?));
        self.push_code(",");

        Ok(())

    }

    pub fn compile(&mut self) -> Result<String, BrainFricError> {
    
        while let Some((line_num, statement)) = self.statements.pop() {

            self.current_line_num = line_num;

            if self.next_register_offset != 0 {
                err!(self.current_line_num, InternalCompilerError::UnfreedRegister);
            }
    
            match statement {
                Statement::Declaration(name, data_type) => 
                    self.handle_declaration(&name, data_type)?,

                Statement::SetTo(name, value) => 
                    self.handle_set_to(&name, value)?,

                Statement::Inc(name) =>
                    self.handle_inc(&name)?,

                Statement::Dec(name) =>
                    self.handle_dec(&name)?,

                Statement::Write(expression) => 
                    self.handle_write(expression)?,

                Statement::Read(name) => 
                    self.handle_read(&name)?
            }
        }

        let mut blank = String::new();
        std::mem::swap(&mut blank, &mut self.compiled);
        Ok(blank)

    }
}