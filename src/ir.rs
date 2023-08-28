use std::collections::HashMap;

use crate::error::*;
use crate::err;
use crate::parse::*;

pub type Register = usize;

#[derive(Debug)]
pub enum IRStatement {
    Alloc(Register, usize, bool),
    Free(Register),
    AddConst(Register, u8),
    MoveCell(Vec<Register>, Register),
    WriteByte(Register),
    ReadByte(Register),
    BeginWhile(Register, bool),
    EndWhile(Register)
}

pub struct IRGenerator {
    ir: Vec<IRStatement>,
    current_line_num: usize,
    next_register: Register,
    name_table: HashMap<String, (Register, DataType)>
}

impl IRGenerator {

    pub fn new() -> Self {
        Self {
            ir: Vec::new(),
            current_line_num: 0,
            next_register: 0,
            name_table: HashMap::new()
        }
    }

    fn alloc(&mut self, data_type: DataType, can_delete: bool) -> Register {
        
        let reg = self.next_register;

        self.ir.push(match data_type {
            DataType::Byte | DataType::Bool => IRStatement::Alloc(reg, 1, can_delete),
            _ => todo!()
        });

        self.next_register += 1;
        reg

    }

    fn alloc_register(&mut self, data_type: DataType) -> Register {
        self.alloc(data_type, true)
    }

    fn alloc_named(&mut self, name: String, data_type: DataType) -> Register {
        let reg = self.alloc(data_type.clone(), false);
        self.name_table.insert(name, (reg, data_type));
        reg
    }

    fn try_free(&mut self, reg: Register, is_reg: bool) {
        if is_reg {
            self.ir.push(IRStatement::Free(reg));
        }
    }

    fn get_name(&self, name: &String) -> Result<(Register, DataType), BrainFricError> {

        if let Some(reg) = self.name_table.get(name) {
            Ok(reg.clone())
        }
        else {
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
            DataType::Byte | DataType::Bool => IRStatement::MoveCell(to.to_vec(), from),
            _ => todo!()
        });
    }

    fn do_copy(&mut self, to: Register, from: Register, data_type: DataType) {
        let reg = self.alloc_register(data_type.clone());
        self.do_move(&[reg, to], from, data_type.clone());
        self.do_move(&[from], reg, data_type);
        self.try_free(reg, true);
    }

    fn do_clear(&mut self, reg: Register, data_type: DataType) {
        self.ir.push(match data_type {
            DataType::Byte | DataType::Bool => IRStatement::MoveCell(vec![], reg),
            _ => todo!()
        });
    }

    fn do_add_const(&mut self, var: Register, val: u8) {
        self.ir.push(IRStatement::AddConst(var, val));
    }

    fn do_sub_const(&mut self, var: Register, val: u8) {
        self.do_add_const(var, (256 - val as u16) as u8);
    }

    fn do_write(&mut self, var: Register) {
        self.ir.push(IRStatement::WriteByte(var));
    }

    fn do_read(&mut self, var: Register) {
        self.ir.push(IRStatement::ReadByte(var));
    }

    fn do_while(&mut self, var: Register, run_once: bool) {
        self.ir.push(IRStatement::BeginWhile(var, run_once));
    }

    fn do_end_while(&mut self, var: Register) {
        self.ir.push(IRStatement::EndWhile(var))
    }

    fn evaluate_expression_into_reg(&mut self, expression: &Expression, expected_type: &DataType, reg: Register) -> Result<(), BrainFricError> {

        let got_type = match expression {
            Expression::Identifier(name) => {
                let (var, data_type) = self.get_name(&name)?;
                self.do_copy(reg, var, expected_type.clone());
                data_type
            }
            Expression::NumberLiteral(value) => {
                self.do_add_const(reg, *value as u8);
                DataType::Byte
            }
            Expression::BoolLiteral(value) => {
                self.do_add_const(reg, *value as u8);
                DataType::Bool
            }
            Expression::Add(expr1, expr2) => {
                self.evaluate_expression_into_reg(expr1, expected_type, reg)?;
                self.evaluate_expression_into_reg(expr2, expected_type, reg)?;
                DataType::Byte
            }
            Expression::AsBool(expr) => {
                // massive TODO
                self.evaluate_expression_into_reg(expr, &DataType::Byte, reg)?;
                DataType::Bool
            }
            _ => todo!()
        };

        self.assert_data_type(expected_type, &got_type)?;

        Ok(())

    }

    fn evaluate_expression(&mut self, expression: &Expression, expected_type: &DataType) -> Result<(Register, bool), BrainFricError> {
        
        Ok(match expression {
            Expression::Identifier(name) => {
                let (reg, data_type) = self.get_name(name)?;
                self.assert_data_type(expected_type, &data_type)?;
                (reg, false)
            }
            Expression::AsBool(expr) if let Expression::Identifier(name) = expr.as_ref() => {
                let (reg, _) = self.get_name(name)?;
                self.assert_data_type(expected_type, &DataType::Bool)?;
                (reg, false)
            }
            _ => {
                let reg = self.alloc_register(expected_type.clone());
                self.evaluate_expression_into_reg(expression, expected_type, reg)?;
                (reg, true)
            }
        })
    }
    
    pub fn generate_ir(&mut self, mut statements: Vec<(usize, Statement)>) -> Result<Vec<IRStatement>, BrainFricError> {

        statements.reverse();
        
        while let Some((line_num, statement)) = statements.pop() {

            self.current_line_num = line_num;
            
            match statement {
                Statement::Declaration(name, data_type) => {
                    self.alloc_named(name, data_type);
                }
                Statement::SetTo(name, expression) => {

                    let (var, data_type) = self.get_name(&name)?;

                    self.do_clear(var, data_type.clone());
                    self.evaluate_expression_into_reg(&expression, &data_type.clone(), var)?;

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
                    let (reg, is_reg) = self.evaluate_expression(&expression, &DataType::Byte)?;
                    self.do_write(reg);
                    self.try_free(reg, is_reg);
                }
                Statement::Read(name) => {
                    let (var, data_type) = self.get_name(&name)?;
                    self.assert_data_type(&data_type, &DataType::Byte)?;
                    self.do_read(var);
                }
                Statement::While(expression, loop_statements) => {

                    let (reg1, is_reg1) = self.evaluate_expression(&expression, &DataType::Bool)?;
                    self.do_while(reg1, false);

                    let loop_ir = self.generate_ir(loop_statements)?;
                    self.ir.extend(loop_ir);

                    let (reg2, is_reg2) = self.evaluate_expression(&expression, &DataType::Bool)?;

                    if reg1 != reg2 {
                        self.do_move(&[reg1], reg2, DataType::Bool);
                    }

                    self.do_end_while(reg1);

                    self.try_free(reg2, is_reg2);
                    self.try_free(reg1, is_reg1);

                }
                Statement::If(expression, loop_statements) => {

                    let reg = self.alloc_register(DataType::Bool);
                    self.evaluate_expression_into_reg(&expression, &DataType::Bool, reg)?;
                    self.do_while(reg, true);

                    let loop_ir = self.generate_ir(loop_statements)?;
                    self.ir.extend(loop_ir);

                    self.do_clear(reg, DataType::Bool);
                    self.do_end_while(reg);

                    self.try_free(reg, true);

                }
            }
        }

        let mut blank = Vec::new();
        std::mem::swap(&mut blank, &mut self.ir);
        Ok(blank)
        
    }
}