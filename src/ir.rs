use std::collections::HashMap;

use crate::error::*;
use crate::err;
use crate::parse::*;

pub type Address = usize;

enum Pointer {
    Variable(Address),
    Register(Address)
}

#[derive(Debug)]
pub enum IRStatement {
    Alloc(Address, usize, bool),
    Free(Address),
    AddConst(Address, u8),
    MoveCell(Vec<Address>, Address),
    SubCell(Vec<Address>, Address),
    WriteByte(Address),
    ReadByte(Address),
    BeginWhile(Address, bool),
    EndWhile(Address),
    MoveBool(Address, Address)
}

pub struct IRGenerator {
    ir: Vec<IRStatement>,
    current_line_num: usize,
    next_register: Address,
    name_table: HashMap<String, (Address, DataType)>
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

    fn alloc(&mut self, data_type: DataType, can_delete: bool) -> Address {
        
        let reg = self.next_register;

        self.ir.push(match data_type {
            DataType::Byte | DataType::Bool => IRStatement::Alloc(reg, 1, can_delete),
            _ => todo!()
        });

        self.next_register += 1;
        reg

    }

    fn alloc_register(&mut self, data_type: DataType) -> Address {
        self.alloc(data_type, true)
    }

    fn alloc_named(&mut self, name: String, data_type: DataType) -> Address {
        let reg = self.alloc(data_type.clone(), false);
        self.name_table.insert(name, (reg, data_type));
        reg
    }

    fn try_free(&mut self, reg: Address, is_reg: bool) {
        
        if is_reg {

            self.do_clear(reg, DataType::Byte);
            self.ir.push(IRStatement::Free(reg));

            // if reg == self.next_register - 1 {
            //     self.next_register -= 1;
            // }
        }
    }

    fn get_name(&self, name: &String) -> Result<(Address, DataType), BrainFricError> {

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

    fn do_move(&mut self, to: &[Address], from: Address, data_type: DataType) {
        self.ir.push(match data_type {
            DataType::Byte | DataType::Bool => IRStatement::MoveCell(to.to_vec(), from),
            _ => todo!()
        });
    }

    fn do_move_negative(&mut self, to: &[Address], from: Address, data_type: DataType) {
        self.ir.push(match data_type {
            DataType::Byte | DataType::Bool => IRStatement::SubCell(to.to_vec(), from),
            _ => todo!()
        });
    }

    fn do_copy(&mut self, to: Address, from: Address, data_type: DataType) {
        let reg = self.alloc_register(data_type.clone());
        self.do_move(&[reg, to], from, data_type.clone());
        self.do_move(&[from], reg, data_type);
        self.try_free(reg, true);
    }

    fn do_copy_negative(&mut self, to: Address, from: Address) {
        let reg = self.alloc_register(DataType::Byte);
        self.do_move(&[reg], from, DataType::Byte);
        self.do_move_negative(&[from, to], reg, DataType::Byte);
        self.try_free(reg, true);
    }

    fn do_move_bool(&mut self, to: Address, from: Address) {
        self.ir.push(IRStatement::MoveBool(to, from));
    }

    fn do_clear(&mut self, reg: Address, data_type: DataType) {
        self.ir.push(match data_type {
            DataType::Byte | DataType::Bool => IRStatement::MoveCell(vec![], reg),
            _ => todo!()
        });
    }

    fn do_add_const(&mut self, reg: Address, val: u8) {
        self.ir.push(IRStatement::AddConst(reg, val));
    }

    fn do_sub_const(&mut self, reg: Address, val: u8) {
        self.do_add_const(reg, (256 - val as u16) as u8);
    }

    fn do_write(&mut self, reg: Address) {
        self.ir.push(IRStatement::WriteByte(reg));
    }

    fn do_read(&mut self, reg: Address) {
        self.ir.push(IRStatement::ReadByte(reg));
    }

    fn do_while(&mut self, reg: Address) {
        self.ir.push(IRStatement::BeginWhile(reg, false));
    }

    fn do_if(&mut self, reg: Address) {
        self.ir.push(IRStatement::BeginWhile(reg, true));
    }

    fn do_end_loop(&mut self, reg: Address) {
        self.ir.push(IRStatement::EndWhile(reg))
    }

    fn evaluate_bool_expression_into_reg(&mut self, expression: &Expression, into_reg: Address) -> Result<(), BrainFricError> {

        let got_type = match expression {
            Expression::Identifier(name) => {
                let (var, data_type) = self.get_name(&name)?;
                self.do_copy(into_reg, var, DataType::Bool);
                data_type
            }
            Expression::BoolLiteral(value) => {
                self.do_add_const(into_reg, *value as u8);
                DataType::Bool
            }
            Expression::AsBool(expr) => {

                let (temp_reg1, is_reg) = self.evaluate_expression(expr, &DataType::Byte)?;
                let temp_reg2 = self.alloc_register(DataType::Byte);

                self.do_move(&[into_reg, temp_reg2], temp_reg1, DataType::Byte);
                self.do_move(&[temp_reg1], into_reg, DataType::Byte);
                self.do_move_bool(into_reg, temp_reg2);
                
                self.try_free(temp_reg2, true);
                self.try_free(temp_reg1, is_reg);

                DataType::Bool

            }
            _ => todo!()
        };

        self.assert_data_type(&DataType::Bool, &got_type)?;

        Ok(())
    }

    fn evaluate_byte_expression_into_reg(&mut self, expression: &Expression, into_reg: Address, negate: bool) -> Result<(), BrainFricError> {

        let got_type = match expression {
            Expression::Identifier(name) => {
                let (var, data_type) = self.get_name(&name)?;
                if negate {
                    self.do_copy_negative(into_reg, var);
                }
                else {
                    self.do_copy(into_reg, var, DataType::Byte);
                }
                data_type
            }
            Expression::NumberLiteral(value) => {
                if negate {
                    self.do_sub_const(into_reg, *value as u8);
                }
                else {
                    self.do_add_const(into_reg, *value as u8);
                }
                DataType::Byte
            }
            Expression::Add(expr1, expr2) => {
                self.evaluate_byte_expression_into_reg(expr1, into_reg, negate)?;
                self.evaluate_byte_expression_into_reg(expr2, into_reg, negate)?;
                DataType::Byte
            }
            Expression::Subtract(expr1, expr2) => {
                self.evaluate_byte_expression_into_reg(expr1, into_reg, negate)?;
                self.evaluate_byte_expression_into_reg(expr2, into_reg, !negate)?;
                DataType::Byte
            }
            _ => todo!()
        };

        self.assert_data_type(&DataType::Byte, &got_type)?;

        Ok(())

    }

    fn evaluate_expression_into_reg(&mut self, expression: &Expression, expected_type: &DataType, into_reg: Address) -> Result<(), BrainFricError> {

        match expected_type {
            DataType::Bool =>
                self.evaluate_bool_expression_into_reg(expression, into_reg),
            DataType::Byte =>
                self.evaluate_byte_expression_into_reg(expression, into_reg, false),
            _ => todo!()
        }
    }

    fn evaluate_expression(&mut self, expression: &Expression, expected_type: &DataType) -> Result<(Address, bool), BrainFricError> {
        
        Ok(match expression {
            Expression::Identifier(name) => {
                let (reg, data_type) = self.get_name(name)?;
                self.assert_data_type(expected_type, &data_type)?;
                (reg, false)
            }
            _ => {
                let reg = self.alloc_register(expected_type.clone());
                self.evaluate_expression_into_reg(expression, expected_type, reg)?;
                (reg, true)
            }
        })
    }
    
    pub fn generate_ir(&mut self, mut statements: Vec<Statement>) -> Result<Vec<IRStatement>, BrainFricError> {

        statements.reverse();
        
        while let Some(statement) = statements.pop() {

            self.current_line_num = statement.line_num;
            
            match statement.body {
                StatementBody::Declaration(name, data_type) => {
                    self.alloc_named(name, data_type);
                }
                StatementBody::SetTo(name, expression) => {

                    let (var, data_type) = self.get_name(&name)?;

                    self.do_clear(var, data_type.clone());
                    self.evaluate_expression_into_reg(&expression, &data_type.clone(), var)?;

                }
                StatementBody::Inc(name) => {
                    let (var, data_type) = self.get_name(&name)?;
                    self.assert_data_type(&data_type, &DataType::Byte)?;
                    self.do_add_const(var, 1);
                }
                StatementBody::Dec(name) => {
                    let (var, data_type) = self.get_name(&name)?;
                    self.assert_data_type(&data_type, &DataType::Byte)?;
                    self.do_sub_const(var, 1);
                }
                StatementBody::Write(expression) => {

                    if let Expression::StringLiteral(literal) = expression {

                        let reg = self.alloc_register(DataType::Byte);

                        let mut val = 0u8;

                        for chr in literal.chars() {
                            self.do_add_const(reg, (chr as u8).wrapping_sub(val));
                            self.do_write(reg);
                            val = chr as u8;
                        }

                        self.try_free(reg, true);
                        continue;
                    
                    }

                    let (reg, is_reg) = self.evaluate_expression(&expression, &DataType::Byte)?;
                    self.do_write(reg);
                    self.try_free(reg, is_reg);

                }
                StatementBody::Read(name) => {
                    let (var, data_type) = self.get_name(&name)?;
                    self.assert_data_type(&data_type, &DataType::Byte)?;
                    self.do_read(var);
                }
                StatementBody::While(expression, loop_statements) => {

                    // optimization for special case
                    if let Expression::AsBool(box Expression::Identifier(name)) = &expression {

                        let (var, _) = self.get_name(name)?;
                        let loop_ir = self.generate_ir(loop_statements)?;

                        self.do_while(var);
                        self.ir.extend(loop_ir); 
                        self.do_end_loop(var);

                        continue;

                    }

                    let (reg1, is_reg1) = self.evaluate_expression(&expression, &DataType::Bool)?;
                    self.do_while(reg1);

                    let loop_ir = self.generate_ir(loop_statements)?;
                    self.ir.extend(loop_ir);

                    let (reg2, is_reg2) = self.evaluate_expression(&expression, &DataType::Bool)?;

                    if reg1 != reg2 {
                        self.do_move(&[reg1], reg2, DataType::Bool);
                    }

                    self.do_end_loop(reg1);

                    self.try_free(reg2, is_reg2);
                    self.try_free(reg1, is_reg1);

                }
                StatementBody::If(expression, loop_statements) => {

                    let reg = self.alloc_register(DataType::Bool);
                    self.evaluate_bool_expression_into_reg(&expression, reg)?;
                    self.do_if(reg);

                    let loop_ir = self.generate_ir(loop_statements)?;
                    self.ir.extend(loop_ir);

                    self.do_clear(reg, DataType::Bool);
                    self.do_end_loop(reg);

                    self.try_free(reg, true);

                }
            }
        }

        Ok(std::mem::take(&mut self.ir))
        
    }
}