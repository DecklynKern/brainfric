use std::collections::{HashMap, HashSet};
use std::mem;

use crate::error::*;
use crate::err;
use crate::parse::*;

// not really an address
pub type Address = usize;

#[derive(PartialEq, Eq, Clone, Copy)]
enum Pointer {
    Variable(Address),
    Register(Address)
}

impl Pointer {
    pub fn get_addr(&self) -> Address {
        match self {
            Self::Variable(address) | Self::Register(address) => *address
        }
    }
}

#[derive(PartialEq, Eq, Default)]
pub struct IRBlock(pub Vec<IRStatement>);

impl IRBlock {

    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn get_used_addresses(&self) -> HashSet<Address> {

        let mut used = HashSet::new();

        for statement in &self.0 {
            used.extend(statement.get_used_addresses().iter());
        }

        used

    }
    
    pub fn uses_address_value(&self, check_address: Address) -> bool {
        self.0.iter().any(|statement| statement.uses_address_value(check_address))
    }

    pub fn references_address(&self, check_address: Address) -> bool {
        self.0.iter().any(|statement| statement.references_address(check_address))
    }

    pub fn delete_address(&mut self, address: Address) -> bool {
        self.0.iter_mut().all(|statement| statement.delete_address(address))
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
    Alloc(Address, usize, bool),
    Free(Address),
    AddConst(Address, u8),
    MoveCell(Vec<Address>, Address),
    SubCell(Vec<Address>, Address),
    WriteByte(Address),
    ReadByte(Address),
    While(Address, IRBlock, bool)
}

impl IRStatement {

    pub fn get_used_addresses(&self) -> HashSet<Address> {

        match self {
            Self::Alloc(address, _, _) | Self::Free(address) | Self::AddConst(address, _) | Self::WriteByte(address) | Self::ReadByte(address)  => {
                HashSet::from([*address])
            },
            Self::MoveCell(to, from) | Self::SubCell(to, from) => {
                let mut addresses = HashSet::from_iter(to.iter().cloned());
                addresses.insert(*from);
                addresses
            }
            Self::While(address, block, _) => {
                let mut addresses = block.get_used_addresses();
                addresses.insert(*address);
                addresses
            }
        }
    }

    pub fn uses_address_value(&self, check_address: Address) -> bool {

        match self {
            Self::MoveCell(_, from) | Self::SubCell(_, from) => *from == check_address,
            Self::WriteByte(address) | Self::ReadByte(address) => *address == check_address, // add compiler flag?
            Self::While(address, block, _) => *address == check_address || block.uses_address_value(check_address),
            Self::Alloc(_, _, _) | Self::AddConst(_, _) | Self::Free(_) => false
        }
    }

    pub fn references_address(&self, check_address: Address) -> bool {
        match self {
            Self::MoveCell(to, from) | Self::SubCell(to, from) =>
                *from == check_address || to.contains(&check_address),
            Self::Alloc(address, _, _) | Self::AddConst(address, _) | Self::WriteByte(address) |
            Self::ReadByte(address) | Self::Free(address) =>
                *address == check_address,
            Self::While(address, block, _) => *address == check_address || block.references_address(check_address)
        }
    }

    pub fn delete_address(&mut self, delete_address: Address) -> bool {

        match self {
            Self::Alloc(address, _, _) | Self::AddConst(address, _) | Self::Free(address)
                => *address == delete_address,
            Self::MoveCell(to, from) | Self::SubCell(to, from) => {
                if *from == delete_address {
                    panic!("attempted to delete used value")
                }
                else {

                    for idx in 0..to.len() {
                        if to[idx] == delete_address {
                            to.remove(idx);
                            break;
                        }
                    }

                    false

                }
            }
            Self::WriteByte(address) | Self::ReadByte(address) => {

                if *address == delete_address {
                    panic!("attempted to delete used value")
                }
                else {
                    false
                }
            }
            Self::While(address, block, _) => {

                if *address == delete_address {
                    panic!("attempted to delete used value")
                }

                block.delete_address(delete_address);

                false

            }
        }
    }
}

pub struct IRGenerator {
    ir: IRBlock,
    loop_stack: Vec<IRBlock>,
    current_line_num: usize,
    next_register: Address,
    name_table: HashMap<String, (Address, DataType)>
}

impl IRGenerator {

    pub fn new() -> Self {
        Self {
            loop_stack: Vec::new(),
            ir: IRBlock::new(),
            current_line_num: 0,
            next_register: 0,
            name_table: HashMap::new()
        }
    }

    fn alloc(&mut self, data_type: DataType, can_delete: bool) -> Address {
        
        let reg = self.next_register;

        self.ir.0.push(match data_type {
            DataType::Byte | DataType::Bool => IRStatement::Alloc(reg, 1, can_delete),
            _ => todo!()
        });

        self.next_register += 1;
        reg

    }

    fn alloc_register(&mut self) -> Pointer {
        Pointer::Register(self.alloc(DataType::Byte, true))
    }

    fn alloc_named(&mut self, name: String, data_type: DataType) -> Pointer {
        let reg = self.alloc(data_type.clone(), false);
        self.name_table.insert(name, (reg, data_type));
        Pointer::Variable(reg)
    }

    fn try_free(&mut self, mem: Pointer) {

        match mem {
            Pointer::Register(address) => {
                self.do_clear(mem);
                self.ir.0.push(IRStatement::Free(address));
            }
            Pointer::Variable(_) => {}
        }
    }

    fn get_name(&self, name: &String) -> Result<(Pointer, DataType), BrainFricError> {

        if let Some((address, data_type)) = self.name_table.get(name) {
            Ok((Pointer::Variable(*address), data_type.clone()))
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

    fn do_move(&mut self, to: &[Pointer], from: Pointer) {
        self.ir.0.push(IRStatement::MoveCell(
            to.iter().map(|ptr| ptr.get_addr()).collect(),
            from.get_addr()
        ));
    }

    fn do_move_negative(&mut self, to: &[Pointer], from: Pointer) {
        self.ir.0.push(IRStatement::SubCell(
            to.iter().map(|ptr| ptr.get_addr()).collect(),
            from.get_addr()
        ));
    }

    fn do_copy(&mut self, to: Pointer, from: Pointer) {
        let reg = self.alloc_register();
        self.do_move(&[reg, to], from);
        self.do_move(&[from], reg);
        self.try_free(reg);
    }

    fn do_copy_negative(&mut self, to: Pointer, from: Pointer) {
        let reg = self.alloc_register();
        self.do_move(&[reg], from);
        self.do_move_negative(&[from, to], reg);
        self.try_free(reg);
    }

    fn do_clear(&mut self, mem: Pointer) {
        self.ir.0.push(IRStatement::MoveCell(vec![], mem.get_addr()));
    }

    fn do_add_const(&mut self, mem: Pointer, val: u8) {
        self.ir.0.push(IRStatement::AddConst(mem.get_addr(), val));
    }

    fn do_sub_const(&mut self, mem: Pointer, val: u8) {
        self.do_add_const(mem, (256 - val as u16) as u8);
    }

    fn do_write(&mut self, mem: Pointer) {
        self.ir.0.push(IRStatement::WriteByte(mem.get_addr()));
    }

    fn do_read(&mut self, mem: Pointer) {
        self.ir.0.push(IRStatement::ReadByte(mem.get_addr()));
    }

    fn do_begin_loop(&mut self) {
        self.loop_stack.push(mem::take(&mut self.ir));
    }

    fn do_end_loop(&mut self, mem: Pointer, is_if: bool) {
        mem::swap(&mut self.ir, &mut self.loop_stack.last_mut().unwrap());
        self.ir.0.push(IRStatement::While(mem.get_addr(), self.loop_stack.pop().unwrap(), is_if));
    }

    fn evaluate_bool_expression_into(&mut self, expression: &Expression, into: Pointer) -> Result<(), BrainFricError> {

        let got_type = match expression {
            Expression::Identifier(name) => {
                let (var, data_type) = self.get_name(&name)?;
                self.do_copy(into, var);
                data_type
            }
            Expression::BoolLiteral(value) => {
                self.do_add_const(into, *value as u8);
                DataType::Bool
            }
            Expression::AsBool(expr) => {

                let mem = self.evaluate_expression(expr, &DataType::Byte)?;
                let reg = self.alloc_register();

                self.do_move(&[into, reg], mem);
                self.do_move(&[mem], into);

                self.do_begin_loop();
                self.do_add_const(into, 1);
                self.do_clear(reg);
                self.do_end_loop(reg, true);
                
                self.try_free(reg);
                self.try_free(mem);

                DataType::Bool

            }
            Expression::Not(expr) => {

                let reg = self.alloc_register();
                self.evaluate_bool_expression_into(expr, reg)?;

                

                DataType::Bool

            }
            Expression::And(expr1, expr2) => {

                let mem1 = self.evaluate_expression(expr1, &DataType::Bool)?;
                let mem2 = self.evaluate_expression(expr2, &DataType::Bool)?;

                let reg = self.alloc_register();

                self.do_copy(reg, mem2);

                self.do_begin_loop();
                self.do_copy(into, mem1);
                self.do_clear(reg);
                self.do_end_loop(reg, true);

                self.try_free(reg);
                self.try_free(mem2);
                self.try_free(mem1);

                DataType::Bool

            }
            Expression::Or(expr1, expr2) => {

                let mem1 = self.evaluate_expression(expr1, &DataType::Bool)?;
                let mem2 = self.evaluate_expression(expr2, &DataType::Bool)?;

                let reg = self.alloc_register();
                self.do_copy(reg, mem1);
                self.do_copy(reg, mem2);

                self.do_begin_loop();
                self.do_add_const(into, 1);
                self.do_clear(reg);
                self.do_end_loop(reg, true);

                self.try_free(reg);
                self.try_free(mem2);
                self.try_free(mem1);

                DataType::Bool

            }
            _ => todo!()
        };

        self.assert_data_type(&DataType::Bool, &got_type)?;

        Ok(())
    }

    fn evaluate_byte_expression_into(&mut self, expression: &Expression, into: Pointer, negate: bool) -> Result<(), BrainFricError> {

        let got_type = match expression {
            Expression::Identifier(name) => {
                let (var, data_type) = self.get_name(&name)?;
                if negate {
                    self.do_copy_negative(into, var);
                }
                else {
                    self.do_copy(into, var);
                }
                data_type
            }
            Expression::NumberLiteral(value) => {
                if negate {
                    self.do_sub_const(into, *value as u8);
                }
                else {
                    self.do_add_const(into, *value as u8);
                }
                DataType::Byte
            }
            Expression::Add(expr1, expr2) => {
                self.evaluate_byte_expression_into(expr1, into, negate)?;
                self.evaluate_byte_expression_into(expr2, into, negate)?;
                DataType::Byte
            }
            Expression::Subtract(expr1, expr2) => {
                self.evaluate_byte_expression_into(expr1, into, negate)?;
                self.evaluate_byte_expression_into(expr2, into, !negate)?;
                DataType::Byte
            }
            _ => todo!()
        };

        self.assert_data_type(&DataType::Byte, &got_type)?;

        Ok(())

    }

    fn evaluate_expression_into(&mut self, expression: &Expression, expected_type: &DataType, into: Pointer) -> Result<(), BrainFricError> {

        match expected_type {
            DataType::Bool =>
                self.evaluate_bool_expression_into(expression, into),
            DataType::Byte =>
                self.evaluate_byte_expression_into(expression, into, false),
            _ => todo!()
        }
    }

    fn evaluate_expression(&mut self, expression: &Expression, expected_type: &DataType) -> Result<Pointer, BrainFricError> {
        
        Ok(match expression {
            Expression::Identifier(name) => {
                let (reg, data_type) = self.get_name(name)?;
                self.assert_data_type(expected_type, &data_type)?;
                reg
            }
            _ => {
                let reg = self.alloc_register();
                self.evaluate_expression_into(expression, expected_type, reg)?;
                reg
            }
        })
    }
    
    pub fn generate_ir(&mut self, mut statements: Vec<Statement>) -> Result<IRBlock, BrainFricError> {

        statements.reverse();
        
        while let Some(statement) = statements.pop() {

            self.current_line_num = statement.line_num;
            
            match statement.body {
                StatementBody::Declaration(name, data_type) => {
                    self.alloc_named(name, data_type);
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

                        let reg = self.alloc_register();

                        let mut val = 0u8;

                        for chr in literal.chars() {
                            self.do_add_const(reg, (chr as u8).wrapping_sub(val));
                            self.do_write(reg);
                            val = chr as u8;
                        }

                        self.try_free(reg);
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

                    let reg1 = self.evaluate_expression(&expression, &DataType::Bool)?;

                    self.do_begin_loop();

                    let loop_ir = self.generate_ir(loop_statements)?;
                    self.ir.0.extend(loop_ir.0);

                    let reg2 = self.evaluate_expression(&expression, &DataType::Bool)?;

                    if reg1 != reg2 {
                        self.do_move(&[reg1], reg2);
                    }

                    self.do_end_loop(reg1, false);

                    self.try_free(reg2);
                    self.try_free(reg1);

                }
                StatementBody::If(expression, loop_statements) => {

                    let reg = self.alloc_register();
                    self.evaluate_bool_expression_into(&expression, reg)?;
                    
                    self.do_begin_loop();

                    let loop_ir = self.generate_ir(loop_statements)?;
                    self.ir.0.extend(loop_ir.0);

                    self.do_clear(reg);
                    self.do_end_loop(reg, true);

                    self.try_free(reg);

                }
            }
        }

        Ok(mem::take(&mut self.ir))
        
    }
}