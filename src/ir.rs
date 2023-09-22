use std::collections::{HashMap, HashSet};
use std::mem;

use crate::args::arg_allow_delete_variables;
use crate::error::*;
use crate::err;
use crate::parse::*;

pub type Identifier = usize;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Memory {
    Variable(Identifier),
    Temporary(Identifier)
}

impl Memory {

    pub fn can_delete(&self) -> bool {

        match self {
            Self::Variable(_) => arg_allow_delete_variables(),
            Self::Temporary(_) => true
        }
    }

    pub fn get_identifier(&self) -> Identifier {
        match self {
            Self::Variable(id) | Self::Temporary(id) => *id
        }
    }
}

#[derive(PartialEq, Eq, Default)]
pub struct IRBlock(pub Vec<IRStatement>);

impl IRBlock {

    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn get_used_identifiers(&self) -> HashSet<Identifier> {

        let mut used = HashSet::new();

        for statement in &self.0 {
            used.extend(statement.get_used_identifiers().iter());
        }

        used

    }
    
    pub fn uses_identifier_value(&self, id: Identifier) -> bool {
        self.0.iter().any(|statement| statement.uses_identifier_value(id))
    }

    pub fn references_identifier(&self, id: Identifier) -> bool {
        self.0.iter().any(|statement| statement.references_identifier(id))
    }

    pub fn delete_identifier(&mut self, id: Identifier) -> bool {
        self.0.iter_mut().all(|statement| statement.delete_identifier(id))
    }
}

impl std::fmt::Debug for IRBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let err = self.0.iter().map(|statement| {write!(f, "\n  ")?; statement.fmt(f)}).collect();
        write!(f, "\n")?;
        err
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum IRStatement {
    Alloc(Memory, usize),
    Free(Identifier),
    AddConst(Identifier, u8),
    MoveCell(Box<[(Identifier, bool)]>, Identifier),
    WriteByte(Identifier),
    ReadByte(Identifier),
    While(Identifier, IRBlock, bool)
}

impl IRStatement {

    pub fn get_used_identifiers(&self) -> HashSet<Identifier> {

        match self {
            Self::Alloc(mem, _) => {
                HashSet::from([mem.get_identifier()])
            }
            Self::Free(id) | Self::AddConst(id, _) | Self::WriteByte(id) | Self::ReadByte(id) => {
                HashSet::from([*id])
            },
            Self::MoveCell(to, from) => {
                let mut ids = HashSet::from_iter(to.iter().map(|cell| cell.0));
                ids.insert(*from);
                ids
            }
            Self::While(id, block, _) => {
                let mut ids = block.get_used_identifiers();
                ids.insert(*id);
                ids
            }
        }
    }

    pub fn uses_identifier_value(&self, id: Identifier) -> bool {

        match self {
            Self::MoveCell(_, from)  => *from == id,
            Self::WriteByte(check_id) | Self::ReadByte(check_id) => *check_id == id,
            Self::While(check_id, block, _) => *check_id == id || block.uses_identifier_value(id),
            Self::Alloc(_, _) | Self::AddConst(_, _) | Self::Free(_) => false
        }
    }

    pub fn references_identifier(&self, id: Identifier) -> bool {

        match self {
            Self::MoveCell(to, from) =>
                *from == id || to.iter().any(|cell| cell.0 == id),
            Self::Alloc(mem, _) =>
                mem.get_identifier() == id,
            Self::AddConst(check_id, _) | Self::WriteByte(check_id) |
            Self::ReadByte(check_id) | Self::Free(check_id) =>
                *check_id == id,
            Self::While(check_id, block, _) => *check_id == id || block.references_identifier(id)
        }
    }

    pub fn delete_identifier(&mut self, id: Identifier) -> bool {

        match self {
            Self::Alloc(mem, _) =>
                mem.get_identifier() == id,
            Self::AddConst(check_id, _) | Self::Free(check_id)
                => *check_id == id,
            Self::MoveCell(to, from) => {
                if *from == id {
                    panic!("attempted to delete used value")
                }
                else {

                    let _ = mem::replace(
                        to,
                        to.iter()
                            .cloned()
                            .filter(|temp| temp.0 != id)
                            .collect::<Box<_>>()
                    );

                    false

                }
            }
            Self::WriteByte(check_id) | Self::ReadByte(check_id) => {

                if *check_id == id {
                    panic!("attempted to delete used value")
                }
                else {
                    false
                }
            }
            Self::While(check_id, block, _) => {

                if *check_id == id {
                    panic!("attempted to delete used value")
                }

                block.delete_identifier(id);

                false

            }
        }
    }
}

pub struct IRGenerator {
    ir: IRBlock,
    loop_stack: Vec<IRBlock>,
    current_line_num: usize,
    next_identifier: Identifier,
    name_table: HashMap<String, (Identifier, DataType)>
}

impl IRGenerator {

    pub fn new() -> Self {
        Self {
            loop_stack: Vec::new(),
            ir: IRBlock::new(),
            current_line_num: 0,
            next_identifier: 0,
            name_table: HashMap::new()
        }
    }

    fn allocate(&mut self, data_type: DataType, can_delete: bool) -> Identifier {
        
        let mem = (if can_delete {Memory::Temporary} else {Memory::Variable})(self.next_identifier);

        self.ir.0.push(match data_type {
            DataType::Byte | DataType::Bool => IRStatement::Alloc(mem, 1),
            _ => todo!()
        });

        self.next_identifier += 1;
        mem.get_identifier()

    }

    fn allocate_temporary(&mut self) -> Memory {
        Memory::Temporary(self.allocate(DataType::Byte, true))
    }

    fn allocate_variable(&mut self, name: String, data_type: DataType) -> Memory {
        let temp = self.allocate(data_type.clone(), arg_allow_delete_variables());
        self.name_table.insert(name, (temp, data_type));
        Memory::Variable(temp)
    }

    fn try_free(&mut self, mem: Memory) {

        match mem {
            Memory::Temporary(id) => {
                self.do_clear(mem);
                self.ir.0.push(IRStatement::Free(id));
            }
            Memory::Variable(_) => {}
        }
    }

    fn get_name(&self, name: &String) -> Result<(Memory, DataType), BrainFricError> {

        if let Some((id, data_type)) = self.name_table.get(name) {
            Ok((Memory::Variable(*id), data_type.clone()))
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

    fn do_move(&mut self, to: &[Memory], from: Memory) {
        self.ir.0.push(IRStatement::MoveCell(
            to.iter().map(|ptr| (ptr.get_identifier(), false)).collect(),
            from.get_identifier()
        ));
    }

    fn do_move_negative(&mut self, to: &[Memory], from: Memory) {
        self.ir.0.push(IRStatement::MoveCell(
            to.iter().map(|ptr| (ptr.get_identifier(), true)).collect(),
            from.get_identifier()
        ));
    }

    fn do_move_raw(&mut self, to: &[(Memory, bool)], from: Memory) {
        self.ir.0.push(IRStatement::MoveCell(
            to.iter()
                .map(|(mem, negate)| (mem.get_identifier(), *negate))
                .collect(),
            from.get_identifier())
        )
    }

    fn do_copy(&mut self, to: Memory, from: Memory) {
        let temp = self.allocate_temporary();
        self.do_move(&[temp, to], from);
        self.do_move(&[from], temp);
        self.try_free(temp);
    }

    fn do_copy_negative(&mut self, to: Memory, from: Memory) {
        let temp = self.allocate_temporary();
        self.do_move(&[temp], from);
        self.do_move_negative(&[from, to], temp);
        self.try_free(temp);
    }

    fn do_clear(&mut self, mem: Memory) {
        self.ir.0.push(IRStatement::MoveCell(Box::default(), mem.get_identifier()));
    }

    fn do_add_const(&mut self, mem: Memory, val: u8) {
        self.ir.0.push(IRStatement::AddConst(mem.get_identifier(), val));
    }

    fn do_sub_const(&mut self, mem: Memory, val: u8) {
        self.do_add_const(mem, val.wrapping_neg());
    }

    fn do_write(&mut self, mem: Memory) {
        self.ir.0.push(IRStatement::WriteByte(mem.get_identifier()));
    }

    fn do_read(&mut self, mem: Memory) {
        self.ir.0.push(IRStatement::ReadByte(mem.get_identifier()));
    }

    fn do_begin_loop(&mut self) {
        self.loop_stack.push(mem::take(&mut self.ir));
    }

    fn do_end_loop(&mut self, mem: Memory, is_if: bool) {
        mem::swap(&mut self.ir, &mut self.loop_stack.last_mut().unwrap());
        self.ir.0.push(IRStatement::While(mem.get_identifier(), self.loop_stack.pop().unwrap(), is_if));
    }

    fn evaluate_bool_expression_into(&mut self, expression: &Expression, into: Memory) -> Result<(), BrainFricError> {

        match expression {

            Expression::Identifier(name) => {
                let (var, data_type) = self.get_name(&name)?;
                self.do_copy(into, var);
                self.assert_data_type(&DataType::Bool, &data_type)?;
            }
            Expression::BoolLiteral(value) => {
                self.do_add_const(into, *value as u8);
            }
            Expression::AsBool(expr) => {

                let mem = self.evaluate_expression(expr, &DataType::Byte)?;
                let temp = self.allocate_temporary();

                self.do_move(&[into, temp], mem);
                self.do_move(&[mem], into);

                self.do_begin_loop();
                self.do_add_const(into, 1);
                self.do_clear(temp);
                self.do_end_loop(temp, true);
                
                self.try_free(temp);
                self.try_free(mem);

            }
            Expression::Not(expr) => {

                let temp = self.allocate_temporary();
                self.evaluate_bool_expression_into(expr, temp)?;

                self.do_add_const(into, 1);

                self.do_begin_loop();
                self.do_sub_const(temp, 1);
                self.do_sub_const(into, 1);
                self.do_end_loop(temp, true);

                self.try_free(temp);

            }
            Expression::And(expr1, expr2) => {

                let mem1 = self.evaluate_expression(expr1, &DataType::Bool)?;
                let mem2 = self.evaluate_expression(expr2, &DataType::Bool)?;

                let temp = self.allocate_temporary();

                self.do_copy(temp, mem2);

                self.do_begin_loop();
                self.do_copy(into, mem1);
                self.do_sub_const(temp, 1);
                self.do_end_loop(temp, true);

                self.try_free(temp);
                self.try_free(mem2);
                self.try_free(mem1);

            }
            Expression::Or(expr1, expr2) => {

                let mem1 = self.evaluate_expression(expr1, &DataType::Bool)?;
                let mem2 = self.evaluate_expression(expr2, &DataType::Bool)?;

                let temp = self.allocate_temporary();
                self.do_copy(temp, mem1);
                self.do_copy(temp, mem2);

                self.do_begin_loop();
                self.do_add_const(into, 1);
                self.do_clear(temp);
                self.do_end_loop(temp, true);

                self.try_free(temp);
                self.try_free(mem2);
                self.try_free(mem1);

            }
            _ => err!(self.current_line_num, IRError::ExpectedTypedExpression(DataType::Bool))
        };

        Ok(())

    }

    fn evaluate_byte_expression_into(&mut self, expression: &Expression, into: Memory, negate: bool) -> Result<(), BrainFricError> {

        match expression {

            Expression::Identifier(name) => {

                let (var, data_type) = self.get_name(&name)?;

                if negate {
                    self.do_copy_negative(into, var);
                }
                else {
                    self.do_copy(into, var);
                }

                self.assert_data_type(&DataType::Byte, &data_type)?;
                
            }
            Expression::AsNum(expr) => {
                self.evaluate_bool_expression_into(expr, into)?;
            }
            Expression::NumberLiteral(value) => {
                if negate {
                    self.do_sub_const(into, *value as u8);
                }
                else {
                    self.do_add_const(into, *value as u8);
                }
            }
            Expression::Add(expr1, expr2) => {
                self.evaluate_byte_expression_into(expr1, into, negate)?;
                self.evaluate_byte_expression_into(expr2, into, negate)?;
            }
            Expression::Subtract(expr1, expr2) => {
                self.evaluate_byte_expression_into(expr1, into, negate)?;
                self.evaluate_byte_expression_into(expr2, into, !negate)?;
            }
            Expression::Multiply(expr1, expr2) => {

                let temp1 = self.allocate_temporary();
                self.evaluate_byte_expression_into(expr1, temp1, false)?;

                let temp2 = self.allocate_temporary();
                self.evaluate_byte_expression_into(expr2, temp2, false)?;

                let temp3 = self.allocate_temporary();

                self.do_begin_loop();

                if negate {
                    self.do_move_raw(&[(into, true), (temp3, false)], temp1);
                }
                else {
                    self.do_move(&[into, temp3], temp1);
                }

                self.do_move(&[temp1], temp3);

                self.do_sub_const(temp2, 1);
                self.do_end_loop(temp2, false);

                self.try_free(temp3);
                self.try_free(temp2);
                self.try_free(temp1);

            }
            _ => err!(self.current_line_num, IRError::ExpectedTypedExpression(DataType::Byte))
        };

        Ok(())

    }

    fn evaluate_expression_into(&mut self, expression: &Expression, expected_type: &DataType, into: Memory) -> Result<(), BrainFricError> {

        match expected_type {
            DataType::Bool =>
                self.evaluate_bool_expression_into(expression, into),
            DataType::Byte =>
                self.evaluate_byte_expression_into(expression, into, false),
            _ => todo!()
        }
    }

    fn evaluate_expression(&mut self, expression: &Expression, expected_type: &DataType) -> Result<Memory, BrainFricError> {
        
        Ok(match expression {
            Expression::Identifier(name) => {
                let (temp, data_type) = self.get_name(name)?;
                self.assert_data_type(expected_type, &data_type)?;
                temp
            }
            _ => {
                let temp = self.allocate_temporary();
                self.evaluate_expression_into(expression, expected_type, temp)?;
                temp
            }
        })
    }
    
    pub fn generate_ir(&mut self, mut statements: Vec<Statement>) -> Result<IRBlock, BrainFricError> {

        statements.reverse();
        
        while let Some(statement) = statements.pop() {

            self.current_line_num = statement.line_num;
            
            match statement.body {

                StatementBody::Declaration(name, data_type) => {
                    self.allocate_variable(name, data_type);
                }
                StatementBody::SetTo(name, expression) => {

                    let (var, data_type) = self.get_name(&name)?;

                    self.do_clear(var);
                    self.evaluate_expression_into(&expression, &data_type, var)?;

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

                        let temp = self.allocate_temporary();

                        let mut val = 0u8;

                        for chr in literal.chars() {
                            self.do_add_const(temp, (chr as u8).wrapping_sub(val));
                            self.do_write(temp);
                            val = chr as u8;
                        }

                        self.try_free(temp);
                        continue;
                    
                    }

                    let pointer = self.evaluate_expression(&expression, &DataType::Byte)?;
                    self.do_write(pointer);
                    self.try_free(pointer);

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
                        
                        self.do_begin_loop();
                        let loop_ir = self.generate_ir(loop_statements)?;
                        self.ir.0.extend(loop_ir.0); 
                        self.do_end_loop(var, false);

                        continue;

                    }

                    let temp1 = self.evaluate_expression(&expression, &DataType::Bool)?;

                    self.do_begin_loop();

                    let loop_ir = self.generate_ir(loop_statements)?;
                    self.ir.0.extend(loop_ir.0);

                    let temp2 = self.evaluate_expression(&expression, &DataType::Bool)?;

                    if temp1 != temp2 {
                        self.do_move(&[temp1], temp2);
                    }

                    self.do_end_loop(temp1, false);

                    self.try_free(temp2);
                    self.try_free(temp1);

                }
                StatementBody::If(expression, loop_statements) => {

                    let temp = self.allocate_temporary();
                    self.evaluate_bool_expression_into(&expression, temp)?;
                    
                    self.do_begin_loop();

                    let loop_ir = self.generate_ir(loop_statements)?;
                    self.ir.0.extend(loop_ir.0);

                    self.do_clear(temp);
                    self.do_end_loop(temp, true);

                    self.try_free(temp);

                }
            }
        }

        Ok(mem::take(&mut self.ir))
        
    }
}