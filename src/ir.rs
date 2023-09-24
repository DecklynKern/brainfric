use std::collections::{HashMap, HashSet};
use std::mem;

use crate::args::arg_allow_delete_variables;
use crate::error::*;
use crate::err;
use crate::lex::Name;
use crate::parse::*;

pub type Identifier = usize;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Allocation {
    Variable(Identifier, DataType),
    Temporary(Identifier)
}

impl Allocation {

    pub fn can_delete(&self) -> bool {
        match self {
            Self::Variable(_, _) => arg_allow_delete_variables(),
            Self::Temporary(_) => true
        }
    }

    pub fn get_identifier(&self) -> Identifier {
        match self {
            Self::Variable(id, _) | Self::Temporary(id) => *id
        }
    }

    pub fn get_size(&self) -> usize {
        match self {
            Self::Variable(_, data_type) => data_type.get_size(),
            Self::Temporary(_) => 1
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum MemoryAccess {
    Identifier(Identifier),
    StackPush(Box<MemoryAccess>),
    StackPop(Box<MemoryAccess>)
}

impl MemoryAccess {

    pub fn get_identifier(&self) -> Identifier {
        match self {
            Self::Identifier(id) =>
                *id,
            Self::StackPush(access) | Self::StackPop(access) =>
                access.get_identifier()
        }
    }
}

#[derive(PartialEq, Eq, Default)]
pub struct IRBlock(pub Vec<IRStatement>);

impl IRBlock {

    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn get_mutated_identifiers(&self) -> HashSet<Identifier> {

        let mut used = HashSet::new();

        for statement in &self.0 {
            used.extend(statement.get_mutated_identifiers().iter());
        }

        used

    }

    pub fn contains_io(&self) -> bool {
        self.0.iter().any(|statement|
            matches!(statement, IRStatement::WriteByte(_) | IRStatement::ReadByte(_))
        )
    }
    
    pub fn reads_identifier_value(&self, id: Identifier) -> bool {
        self.0.iter().any(|statement| statement.reads_identifier_value(id))
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

        let err = self.0.iter().try_for_each(|statement| {
            write!(f, "\n  ")?;
            statement.fmt(f)
        });
        
        writeln!(f)?;
        err

    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum IRStatement {
    Alloc(Allocation),
    Free(Identifier),
    AddConst(MemoryAccess, u8),
    MoveCell(Box<[(MemoryAccess, bool)]>, MemoryAccess),
    WriteByte(MemoryAccess),
    ReadByte(MemoryAccess),
    Loop(MemoryAccess, IRBlock, bool)
}

impl IRStatement {

    pub fn get_mutated_identifiers(&self) -> HashSet<Identifier> {

        match self {
            Self::Alloc(_) | Self::Free(_) | Self::WriteByte(_) | Self::ReadByte(_) =>
                HashSet::new(),
            Self::AddConst(access, _) => {
                HashSet::from([access.get_identifier()])
            }
            Self::MoveCell(to, from) => {
                let mut ids = HashSet::from_iter(to.iter().map(|cell| cell.0.get_identifier()));
                ids.insert(from.get_identifier());
                ids
            }
            Self::Loop(_, block, _) =>
                block.get_mutated_identifiers()
        }
    }

    pub fn reads_identifier_value(&self, id: Identifier) -> bool {

        match self {
            Self::MoveCell(_, from)  =>
                from.get_identifier() == id,
            Self::WriteByte(access) | Self::ReadByte(access) =>
                access.get_identifier() == id,
            Self::Loop(access, block, _) =>
                access.get_identifier() == id || block.reads_identifier_value(id),
            Self::Alloc(_) | Self::AddConst(_, _) | Self::Free(_) =>
                false
        }
    }

    pub fn references_identifier(&self, id: Identifier) -> bool {

        match self {
            Self::MoveCell(to, from) =>
                from.get_identifier() == id || to.iter().any(|cell| cell.0.get_identifier() == id),
            Self::Alloc(mem) =>
                mem.get_identifier() == id,
            Self::AddConst(access, _) | Self::WriteByte(access) |
            Self::ReadByte(access) =>
                access.get_identifier() == id,
            Self::Free(check_id) =>
                *check_id == id,
            Self::Loop(check_id, block, _) =>
                check_id.get_identifier() == id || block.references_identifier(id)
        }
    }

    pub fn delete_identifier(&mut self, id: Identifier) -> bool {

        match self {
            Self::Alloc(mem) =>
                mem.get_identifier() == id,
            Self::AddConst(access, _) =>
                access.get_identifier() == id,
            Self::Free(check_id) =>
                *check_id == id,
            Self::MoveCell(to, from) => {

                if from.get_identifier() == id {
                    panic!("attempted to delete used value")
                }

                let _ = mem::replace(
                    to,
                    to.iter()
                        .cloned()
                        .filter(|temp| temp.0.get_identifier() != id)
                        .collect::<Box<_>>()
                );

                false

            }
            Self::WriteByte(access) | Self::ReadByte(access) => {

                if access.get_identifier() == id {
                    panic!("attempted to delete used value")
                }
                    
                false

            }
            Self::Loop(access, block, _) => {

                if access.get_identifier() == id {
                    panic!("attempted to delete used value")
                }

                block.delete_identifier(id);

                false

            }
        }
    }
}

pub fn generate_ir(statements: Vec<Statement>) -> Result<IRBlock, BrainFricError> {

    let mut ir_generator = IRGenerator {
        loop_stack: Vec::new(),
        ir: IRBlock::new(),
        current_line_num: 0,
        next_identifier: 0,
        name_table: HashMap::new()
    };

    ir_generator.generate_ir(statements)
    
}

struct IRGenerator {
    ir: IRBlock,
    loop_stack: Vec<IRBlock>,
    current_line_num: usize,
    next_identifier: Identifier,
    name_table: HashMap<Name, (Identifier, DataType)>
}

impl IRGenerator {

    fn do_free(&mut self, id: Identifier) {
        self.do_clear(MemoryAccess::Identifier(id));
        self.ir.0.push(IRStatement::Free(id));
    }

    fn do_free_access(&mut self, access: MemoryAccess) {
        self.do_free(access.get_identifier());
    }

    fn try_free_access(&mut self, access: MemoryAccess, is_temp: bool) {
        if is_temp {
            let id = access.get_identifier();
            self.do_clear(MemoryAccess::Identifier(id));
            self.ir.0.push(IRStatement::Free(id));
        }
    } 

    fn get_name(&self, name: &Name) -> Result<(MemoryAccess, DataType), BrainFricError> {
        self.name_table.get(name.as_ref()).map_or_else(
            ||
                err!(self.current_line_num, IRError::UnknownIdentifier(name.clone())),
            |(id, data_type)|
                Ok((MemoryAccess::Identifier(*id), data_type.clone()))
        )
    }

    fn assert_data_type(&self, expected_type: &DataType, got_type: &DataType) -> Result<(), BrainFricError> {

        if *expected_type != *got_type {
            err!(self.current_line_num, IRError::TypeMismatch(expected_type.clone(), got_type.clone()))
        }

        Ok(())

    }

    fn allocate_temporary(&mut self) -> MemoryAccess {
        
        let mem = Allocation::Temporary(self.next_identifier);
        self.ir.0.push(IRStatement::Alloc(mem));

        self.next_identifier += 1;
        MemoryAccess::Identifier(mem.get_identifier())

    }

    fn allocate_variable(&mut self, name: Name, data_type: DataType) -> MemoryAccess {

        let mem = Allocation::Variable(self.next_identifier, data_type);
        self.ir.0.push(IRStatement::Alloc(mem));

        self.next_identifier += 1;
        MemoryAccess::Identifier(mem.get_identifier())

    }

    fn convert_accessor(&self, accessor: Accessor) -> Result<(MemoryAccess, DataType), BrainFricError> {

        Ok(match accessor {
            Accessor::Identifier(name) => self.get_name(&name)?,
            Accessor::Push(inner_accessor) => {
                let (access, data_type) = self.convert_accessor(*inner_accessor)?;
                (MemoryAccess::StackPush(Box::new(access)), data_type)
            }
            Accessor::Pop(inner_accessor) => {
                let (access, data_type) = self.convert_accessor(*inner_accessor)?;
                (MemoryAccess::StackPop(Box::new(access)), data_type)
            }
        })
    }

    fn assert_can_read(&self, access: &MemoryAccess) -> Result<(), BrainFricError> {
        if !matches!(access, MemoryAccess::StackPush(_)) {
            Ok(())
        }
        else {
            err!(self.current_line_num, IRError::ExpectedReadableAccess)
        }
    }

    fn assert_can_write(&self, access: &MemoryAccess) -> Result<(), BrainFricError> {
        if !matches!(access, MemoryAccess::StackPop(_)) {
            Ok(())
        }
        else {
            err!(self.current_line_num, IRError::ExpectedWriteableAccess)
        }
    }

    fn assert_can_modify(&self, access: &MemoryAccess) -> Result<(), BrainFricError> {
        if matches!(access, MemoryAccess::Identifier(_)) {
            Ok(())
        }
        else {
            err!(self.current_line_num, IRError::ExpectedModifyableAccess)
        }
    }

    fn do_move_raw(&mut self, to: &[(MemoryAccess, bool)], from: MemoryAccess) {
        self.ir.0.push(IRStatement::MoveCell(
            to.iter()
                .map(|(mem, negate)| (*mem, *negate))
                .collect(),
            from)
        )
    }

    fn do_move(&mut self, to: &[MemoryAccess], from: MemoryAccess) {
        self.ir.0.push(IRStatement::MoveCell(
            to.iter().map(|access| (*access, false)).collect(),
            from
        ));
    }

    fn do_move_negative(&mut self, to: &[MemoryAccess], from: MemoryAccess) {
        self.ir.0.push(IRStatement::MoveCell(
            to.iter().map(|access| (*access, true)).collect(),
            from
        ));
    }

    fn do_copy(&mut self, to: MemoryAccess, from: MemoryAccess) {
        let temp = self.allocate_temporary();
        self.do_move(&[temp, to], from);
        self.do_move(&[from], temp);
        self.do_free(temp.get_identifier());
    }

    fn do_copy_negative(&mut self, to: MemoryAccess, from: MemoryAccess) {
        let temp = self.allocate_temporary();
        self.do_move(&[temp], from);
        self.do_move_negative(&[from, to], temp);
        self.do_free(temp.get_identifier());
    }

    fn do_clear(&mut self, access: MemoryAccess) {
        self.ir.0.push(IRStatement::MoveCell(Box::default(), access));
    }

    fn do_add_const(&mut self, access: MemoryAccess, val: u8) {
        self.ir.0.push(IRStatement::AddConst(access, val));
    }

    fn do_sub_const(&mut self, access: MemoryAccess, val: u8) {
        self.do_add_const(access, val.wrapping_neg());
    }

    fn do_write(&mut self, access: MemoryAccess) {
        self.ir.0.push(IRStatement::WriteByte(access));
    }

    fn do_read(&mut self, access: MemoryAccess) {
        self.ir.0.push(IRStatement::ReadByte(access));
    }

    fn do_begin_loop(&mut self) {
        self.loop_stack.push(mem::take(&mut self.ir));
    }

    fn do_end_loop(&mut self, access: MemoryAccess, is_if: bool) {
        mem::swap(&mut self.ir, self.loop_stack.last_mut().unwrap());
        self.ir.0.push(IRStatement::Loop(access, self.loop_stack.pop().unwrap(), is_if));
    }

    fn evaluate_bool_expression_into(&mut self, expression: &Expression, into: MemoryAccess) -> Result<(), BrainFricError> {

        match expression {

            Expression::Access(Accessor::Identifier(name)) => {
                let (var, data_type) = self.get_name(name)?;
                self.do_copy(into, var);
                self.assert_data_type(&DataType::Bool, &data_type)?;
            }
            Expression::BoolLiteral(value) => {
                self.do_add_const(into, *value as u8);
            }
            Expression::AsBool(expr) => {

                let (access, is_temp) = self.evaluate_expression(expr, &DataType::Byte)?;
                let temp = self.allocate_temporary();

                self.do_move(&[into, temp], access);
                self.do_move(&[access], into);

                self.do_begin_loop();
                self.do_add_const(into, 1);
                self.do_clear(temp);
                self.do_end_loop(temp, true);
                
                self.do_free(temp.get_identifier());
                self.try_free_access(access, is_temp);

            }
            Expression::Not(expr) => {

                let temp = self.allocate_temporary();
                self.evaluate_bool_expression_into(expr, temp)?;

                self.do_add_const(into, 1);

                self.do_begin_loop();
                self.do_sub_const(temp, 1);
                self.do_sub_const(into, 1);
                self.do_end_loop(temp, true);

                self.do_free(temp.get_identifier());

            }
            Expression::And(expr1, expr2) => {

                let (access1, is_temp1) = self.evaluate_expression(expr1, &DataType::Bool)?;
                let (access2, is_temp2) = self.evaluate_expression(expr2, &DataType::Bool)?;

                let temp = self.allocate_temporary();

                self.do_copy(temp, access2);

                self.do_begin_loop();
                self.do_copy(into, access1);
                self.do_sub_const(temp, 1);
                self.do_end_loop(temp, true);

                self.do_free(temp.get_identifier());
                self.try_free_access(access2, is_temp2);
                self.try_free_access(access1, is_temp1);

            }
            Expression::Or(expr1, expr2) => {

                let (access1, is_temp1) = self.evaluate_expression(expr1, &DataType::Bool)?;
                let (access2, is_temp2) = self.evaluate_expression(expr2, &DataType::Bool)?;

                let temp = self.allocate_temporary();
                self.do_copy(temp, access1);
                self.do_copy(temp, access2);

                self.do_begin_loop();
                self.do_add_const(into, 1);
                self.do_clear(temp);
                self.do_end_loop(temp, true);

                self.do_free(temp.get_identifier());
                self.try_free_access(access2, is_temp2);
                self.try_free_access(access1, is_temp1);

            }
            _ => err!(self.current_line_num, IRError::ExpectedTypedExpression(DataType::Bool))
        };

        Ok(())

    }

    fn evaluate_byte_expression_into(&mut self, expression: &Expression, into: MemoryAccess, negate: bool) -> Result<(), BrainFricError> {

        match expression {

            Expression::Access(Accessor::Identifier(name)) => {

                let (var, data_type) = self.get_name(name)?;

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

                self.do_free_access(temp3);
                self.do_free_access(temp2);
                self.do_free_access(temp1);

            }
            _ => err!(self.current_line_num, IRError::ExpectedTypedExpression(DataType::Byte))
        };

        Ok(())

    }

    fn evaluate_expression_into(&mut self, expression: &Expression, expected_type: &DataType, into: MemoryAccess) -> Result<(), BrainFricError> {

        match expected_type {
            DataType::Bool =>
                self.evaluate_bool_expression_into(expression, into),
            DataType::Byte =>
                self.evaluate_byte_expression_into(expression, into, false),
            _ => todo!()
        }
    }

    fn evaluate_expression(&mut self, expression: &Expression, expected_type: &DataType) -> Result<(MemoryAccess, bool), BrainFricError> {
        
        Ok(match expression {
            Expression::Access(Accessor::Identifier(name)) => {
                let (mem, data_type) = self.get_name(name)?;
                self.assert_data_type(expected_type, &data_type)?;
                (mem, false)
            }
            _ => {
                let mem = self.allocate_temporary();
                self.evaluate_expression_into(expression, expected_type, mem)?;
                (mem, true)
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
                StatementBody::SetTo(accessor, expression) => {
                    let (access, data_type) = self.convert_accessor(accessor)?;
                    self.assert_can_write(&access)?;
                    self.do_clear(access);
                    self.evaluate_expression_into(&expression, &data_type, access)?;

                }
                StatementBody::Inc(accessor) => {
                    let (access, data_type) = self.convert_accessor(accessor)?;
                    self.assert_can_modify(&access);
                    self.assert_data_type(&data_type, &DataType::Byte)?;
                    self.do_add_const(access, 1);

                }
                StatementBody::Dec(accessor) => {
                    let (access, data_type) = self.convert_accessor(accessor)?;
                    self.assert_can_modify(&access);
                    self.assert_data_type(&data_type, &DataType::Byte)?;
                    self.do_sub_const(access, 1);
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

                        self.do_free_access(temp);
                        continue;
                    
                    }

                    let (access, is_temp) = self.evaluate_expression(&expression, &DataType::Byte)?;
                    self.do_write(access);
                    self.try_free_access(access, is_temp);

                }
                StatementBody::Read(accessor) => {
                    let (access, data_type) = self.convert_accessor(accessor)?;
                    self.assert_can_write(&access)?;
                    self.assert_data_type(&data_type, &DataType::Byte)?;
                    self.do_read(access);
                }
                StatementBody::While(expression, loop_statements) => {

                    // optimization for special case
                    if let Expression::AsBool(box Expression::Access(Accessor::Identifier(name))) = &expression {

                        let (var, _) = self.get_name(name)?;
                        
                        self.do_begin_loop();
                        let loop_ir = self.generate_ir(loop_statements)?;
                        self.ir.0.extend(loop_ir.0); 
                        self.do_end_loop(var, false);

                        continue;

                    }

                    let (access1, is_temp1) = self.evaluate_expression(&expression, &DataType::Bool)?;

                    self.do_begin_loop();

                    let loop_ir = self.generate_ir(loop_statements)?;
                    self.ir.0.extend(loop_ir.0);

                    let (access2, is_temp2) = self.evaluate_expression(&expression, &DataType::Bool)?;

                    if access1 != access2 {
                        self.do_move(&[access1], access2);
                    }

                    self.do_end_loop(access1, false);

                    self.try_free_access(access2, is_temp2);
                    self.try_free_access(access1, is_temp1);

                }
                StatementBody::If(expression, loop_statements) => {

                    let temp = self.allocate_temporary();
                    self.evaluate_bool_expression_into(&expression, temp)?;
                    
                    self.do_begin_loop();

                    let loop_ir = self.generate_ir(loop_statements)?;
                    self.ir.0.extend(loop_ir.0);

                    self.do_clear(temp);
                    self.do_end_loop(temp, true);

                    self.do_free(temp.get_identifier());

                }
            }
        }

        Ok(mem::take(&mut self.ir))
        
    }
}