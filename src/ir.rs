use std::collections::HashMap;

use crate::error::*;
use crate::err;
use crate::parse::*;

type Register = usize;

pub enum IRStatement {
    AllocBool(Register),
    AllocByte(Register),
    Free(Register),
    ClearByte(Register),
    ClearBool(Register),
    AddConst(Register, u8),
    MoveByte(Vec<Register>, Register),
    SubByte(Register),
    MoveBool(Vec<Register>, Register),
    WriteByte(Register),
    ReadByte(Register)
}

struct IRGenerator {
    ir: Vec<IRStatement>,
    current_line_num: usize,
    next_register: Register,
    name_table: HashMap<String, (Register, DataType)>
}

impl IRGenerator {

    fn new() -> Self {
        Self {
            ir: Vec::new(),
            current_line_num: 0,
            next_register: 0,
            name_table: HashMap::new()
        }
    }

    fn alloc(&mut self, data_type: DataType) -> Register {
        
        let reg = self.next_register;

        self.ir.push(match data_type {
            DataType::Byte => IRStatement::AllocByte(reg),
            DataType::Bool => IRStatement::AllocBool(reg),
            _ => todo!()
        });

        self.next_register += 1;
        reg

    }

    fn alloc_named(&mut self, name: String, data_type: DataType) -> Register {
        let reg = self.alloc(data_type.clone());
        self.name_table.insert(name, (reg, data_type));
        reg
    }

    fn free(&mut self, reg: Register) {

        if reg != self.next_register - 1 {
            todo!()
        }

        self.next_register -= 1;

    }

    fn get_name(&self, name: &String) -> Result<(Register, DataType), BrainFricError> {

        if let Some(reg) = self.name_table.get(name) {
            Ok(reg.clone())

        } else {
            err!(self.current_line_num, IRError::UnknownIdentifier(name.clone()))
        }
    }

    fn assert_data_type(&self, expected_type: &DataType, got_type: &DataType) -> Result<(), BrainFricError> {

        if *expected_type != *got_type {
            err!(self.current_line_num, IRError::TypeMismatch(expected_type.clone(), got_type.clone()))
        }

        Ok(())

    }

    fn do_move(&mut self, to: &[Register], from: Register, data_type: DataType) {
        self.ir.push(match data_type {
            DataType::Byte => IRStatement::MoveByte(to.to_vec(), from),
            DataType::Bool => IRStatement::MoveBool(to.to_vec(), from),
            _ => todo!()
        });
    }

    fn do_copy(&mut self, to: Register, from: Register, data_type: DataType) {
        let reg = self.alloc(data_type.clone());
        self.do_move(&[reg, to], from, data_type);
        self.free(reg);
    }

    fn do_clear(&mut self, reg: Register, data_type: DataType) {
        self.ir.push(match data_type {
            DataType::Byte => IRStatement::ClearByte(reg),
            DataType::Bool => IRStatement::ClearBool(reg),
            _ => todo!()
        });
    }

    fn do_add_const(&mut self, var: Register, val: u8) {
        self.ir.push(IRStatement::AddConst(var, val));
    }

    fn do_sub_const(&mut self, var: Register, val: u8) {
        self.do_add_const(var, 255 - val);
    }

    fn do_write(&mut self, var: Register) {
        self.ir.push(IRStatement::WriteByte(var));
    }

    fn do_read(&mut self, var: Register) {
        self.ir.push(IRStatement::WriteByte(var));
    }


    fn evaluate_expression(&mut self, expression: Expression, expected_type: &DataType) -> Result<Register, BrainFricError> {

        let (reg, got_type) = match expression {
            Expression::Identifier(name) => {

                let (var, data_type) = self.get_name(&name)?;
                let reg = self.alloc(data_type.clone());

                self.do_copy(var, reg, data_type.clone());

                (reg, data_type)
                
            }
            Expression::NumberLiteral(value) => {
                let reg = self.alloc(DataType::Byte);
                self.do_add_const(reg, value as u8);
                (reg, DataType::Byte)
            }
            Expression::BoolLiteral(value) => {
                let reg = self.alloc(DataType::Bool);
                self.do_add_const(reg, value as u8);
                (reg, DataType::Bool)
            }
            Expression::Add(expr1, expr2) => {

                let reg1 = self.evaluate_expression(*expr1, expected_type)?;
                let reg2 = self.evaluate_expression(*expr2, expected_type)?;

                let reg3 = self.alloc(DataType::Byte);

                self.do_move(&[reg3], reg1, expected_type.clone());
                self.do_move(&[reg3], reg2, expected_type.clone());

                (reg3, DataType::Byte)

            }
            _ => todo!()
        };

        self.assert_data_type(expected_type, &got_type)?;

        Ok(reg)

    }
    
    pub fn generate_ir(&mut self, mut statements: Vec<(usize, Statement)>) -> Result<Vec<IRStatement>, BrainFricError> {
        
        while let Some((line_num, statement)) = statements.pop() {

            self.current_line_num = line_num;
            
            match statement {
                Statement::Declaration(name, data_type) => {
                    self.alloc_named(name, data_type);
                }
                Statement::SetTo(name, expression) => {

                    let (var, data_type) = self.get_name(&name)?;

                    self.do_clear(var, data_type.clone());

                    let reg = self.evaluate_expression(expression, &data_type.clone())?;

                    self.do_move(&[var], reg, data_type);

                    self.free(reg);

                }
                Statement::Inc(name) => {
                    let (var, data_type) = self.get_name(&name)?;
                    self.assert_data_type(&data_type, &DataType::Byte)?;
                    self.do_add_const(var, 1);
                }
                Statement::Dec(name) => {
                    let (var, data_type) = self.get_name(&name)?;
                    self.assert_data_type(&data_type, &DataType::Byte)?;
                    self.do_sub_const(var, 1);
                }
                Statement::Write(expression) => {
                    let reg = self.evaluate_expression(expression, &DataType::Byte)?;
                    self.do_write(reg);
                    self.free(reg);
                }
                Statement::Read(name) => {
                    let (var, data_type) = self.get_name(&name)?;
                    self.assert_data_type(&data_type, &DataType::Byte)?;
                    self.do_read(var);
                }
            }
        }

        let mut blank = Vec::new();
        std::mem::swap(&mut blank, &mut self.ir);
        Ok(blank)
        
    }
}