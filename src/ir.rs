use std::collections::{HashMap, HashSet};
use std::mem::{swap, take, replace};

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

#[derive(PartialEq, Eq, Clone)]
pub struct MemoryAccess {
    pub identifier: Identifier,
    pub specifiers: Box<[Specifier]>
}

impl std::fmt::Debug for MemoryAccess {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "MemAccess({}, {:?})", self.identifier, self.specifiers)
    }
}

impl MemoryAccess {

    pub fn from_identifier(id: Identifier) -> Self {
        Self {
            identifier: id,
            specifiers: Box::new([])
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
            Self::AddConst(mem, _) => {
                HashSet::from([mem.identifier])
            }
            Self::MoveCell(to, from) => {
                let mut ids = HashSet::from_iter(to.iter().map(|cell| cell.0.identifier));
                ids.insert(from.identifier);
                ids
            }
            Self::Loop(_, block, _) =>
                block.get_mutated_identifiers()
        }
    }

    pub fn reads_identifier_value(&self, id: Identifier) -> bool {

        match self {
            Self::MoveCell(_, from)  =>
                from.identifier == id,
            Self::WriteByte(mem) | Self::ReadByte(mem) =>
                mem.identifier == id,
            Self::Loop(mem, block, _) =>
                mem.identifier == id || block.reads_identifier_value(id),
            Self::Alloc(_) | Self::AddConst(_, _) | Self::Free(_) =>
                false
        }
    }

    pub fn references_identifier(&self, id: Identifier) -> bool {

        match self {
            Self::MoveCell(to, from) =>
                from.identifier == id || to.iter().any(|cell| cell.0.identifier == id),
            Self::Alloc(alloc) =>
                alloc.get_identifier() == id,
            Self::AddConst(mem, _) | Self::WriteByte(mem) |
            Self::ReadByte(mem) =>
                mem.identifier == id,
            Self::Free(check_id) =>
                *check_id == id,
            Self::Loop(check_id, block, _) =>
                check_id.identifier == id || block.references_identifier(id)
        }
    }

    pub fn delete_identifier(&mut self, id: Identifier) -> bool {

        match self {
            Self::Alloc(alloc) =>
                alloc.get_identifier() == id,
            Self::AddConst(mem, _) =>
                mem.identifier == id,
            Self::Free(check_id) =>
                *check_id == id,
            Self::MoveCell(to, from) => {

                if from.identifier == id {
                    panic!("attempted to delete used value")
                }

                let _ = replace(
                    to,
                    to.iter()
                        .cloned()
                        .filter(|temp| temp.0.identifier != id)
                        .collect::<Box<_>>()
                );

                false

            }
            Self::WriteByte(mem) | Self::ReadByte(mem) => {

                if mem.identifier == id {
                    panic!("attempted to delete used value")
                }
                    
                false

            }
            Self::Loop(mem, block, _) => {

                if mem.identifier == id {
                    panic!("attempted to delete used value")
                }

                block.delete_identifier(id);

                false

            }
        }
    }

    fn get_unitary_identifier(&self) -> Option<Identifier> {
        
        match self {
            Self::Alloc(alloc) => Some(alloc.get_identifier()),
            Self::AddConst(mem, _) | Self::WriteByte(mem) => Some(mem.identifier),
            _ => None
        }
    }

    pub fn can_swap_previous(&self, previous: &Self) -> bool {

        self.get_unitary_identifier().is_some_and(
            |id_this| previous.get_unitary_identifier().is_some_and(
                |id_other| id_this < id_other
            )
        )
    }
}

pub fn generate_ir(statements: Vec<Statement>) -> Result<IRBlock, BrainFricError> {

    let mut ir_generator = IRGenerator {
        loop_stack: Vec::new(),
        ir: IRBlock::new(),
        current_line_num: 0,
        next_identifier: 1,
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
        self.do_clear(MemoryAccess::from_identifier(id));
        self.ir.0.push(IRStatement::Free(id));
    }

    fn do_free_access(&mut self, mem: MemoryAccess) {
        self.do_free(mem.identifier);
    }

    fn try_free_access(&mut self, mem: MemoryAccess, is_temp: bool) {
        if is_temp {
            let id = mem.identifier;
            self.do_clear(MemoryAccess::from_identifier(id));
            self.ir.0.push(IRStatement::Free(id));
        }
    }

    fn assert_data_type(&self, expected_type: &DataType, got_type: &DataType) -> Result<(), BrainFricError> {

        if *expected_type != *got_type {
            err!(self.current_line_num, IRError::TypeMismatch(expected_type.clone(), got_type.clone()))
        }

        Ok(())

    }

    fn allocate_temporary(&mut self) -> MemoryAccess {
        
        let alloc = Allocation::Temporary(self.next_identifier);
        self.ir.0.push(IRStatement::Alloc(alloc));
        
        self.next_identifier += 1;
        MemoryAccess::from_identifier(self.next_identifier - 1)
        
    }

    fn allocate_variable(&mut self, name: Name, data_type: DataType) -> MemoryAccess {
        
        self.ir.0.push(IRStatement::Alloc(Allocation::Variable(self.next_identifier, data_type.clone())));
        self.name_table.insert(name, (self.next_identifier, data_type));

        self.next_identifier += 1;
        MemoryAccess::from_identifier(self.next_identifier - 1)

    }

    fn resolve_accessor(&self, accessor: Accessor) -> Result<(MemoryAccess, DataType), BrainFricError> {

        let (id, mut data_type) = self.name_table.get(accessor.identifier.as_ref()).map_or_else(
            ||
                err!(self.current_line_num, IRError::UnknownIdentifier(accessor.identifier)),
            |(id, data_type)|
                Ok((id, data_type.clone()))
        )?;

        for specifier in accessor.specifiers.iter() {
            match (specifier, data_type) {
                (Specifier::StackTop, DataType::Stack(_)) => {
                    data_type = DataType::Byte;
                }
                _ => todo!("bad access")
            }
        }

        Ok((
            MemoryAccess {
                identifier: *id,
                specifiers: accessor.specifiers
            },
            data_type
        ))
    }

    fn assert_can_read(&self, mem: &MemoryAccess) -> Result<(), BrainFricError> {
        Ok(())
        // ?
    }

    fn assert_can_write(&self, mem: &MemoryAccess) -> Result<(), BrainFricError> {
        Ok(())
        // ?
    }

    fn assert_can_modify(&self, mem: &MemoryAccess) -> Result<(), BrainFricError> {
        Ok(())
        // ?
    }

    fn do_move_raw<const N: usize>(&mut self, to: [(MemoryAccess, bool); N], from: MemoryAccess) {
        self.ir.0.push(IRStatement::MoveCell(
            to.iter()
                .map(|(mem, negate)| (mem.clone(), *negate))
                .collect(),
            from)
        )
    }

    fn do_move<const N: usize>(&mut self, to: [MemoryAccess; N], from: MemoryAccess) {
        self.ir.0.push(IRStatement::MoveCell(
            to.into_iter().map(|mem| (mem, false)).collect(),
            from
        ));
    }

    fn do_move_negative<const N: usize>(&mut self, to: [MemoryAccess; N], from: MemoryAccess) {
        self.ir.0.push(IRStatement::MoveCell(
            to.into_iter().map(|mem| (mem, true)).collect(),
            from
        ));
    }

    fn do_copy(&mut self, to: MemoryAccess, from: MemoryAccess) {
        let temp = self.allocate_temporary();
        self.do_move([temp.clone(), to], from.clone());
        self.do_move([from], temp.clone());
        self.do_free(temp.identifier);
    }

    fn do_copy_negative(&mut self, to: MemoryAccess, from: MemoryAccess) {
        let temp = self.allocate_temporary();
        self.do_move([temp.clone()], from.clone());
        self.do_move_negative([from, to], temp.clone());
        self.do_free(temp.identifier);
    }

    fn do_clear(&mut self, mem: MemoryAccess) {
        self.ir.0.push(IRStatement::MoveCell(Box::default(), mem));
    }

    fn do_add_const(&mut self, mem: MemoryAccess, val: u8) {
        self.ir.0.push(IRStatement::AddConst(mem, val));
    }

    fn do_sub_const(&mut self, mem: MemoryAccess, val: u8) {
        self.do_add_const(mem, val.wrapping_neg());
    }

    fn do_write(&mut self, mem: MemoryAccess) {
        self.ir.0.push(IRStatement::WriteByte(mem));
    }

    fn do_read(&mut self, mem: MemoryAccess) {
        self.ir.0.push(IRStatement::ReadByte(mem));
    }

    fn do_begin_loop(&mut self) {
        self.loop_stack.push(take(&mut self.ir));
    }

    fn do_end_loop(&mut self, mem: MemoryAccess, is_if: bool) {
        swap(&mut self.ir, self.loop_stack.last_mut().unwrap());
        self.ir.0.push(IRStatement::Loop(mem, self.loop_stack.pop().unwrap(), is_if));
    }

    fn evaluate_bool_expression_into(&mut self, expression: Expression, into: MemoryAccess) -> Result<(), BrainFricError> {

        match expression {

            Expression::Access(accessor) => {
                let (mem, data_type) = self.resolve_accessor(accessor)?;
                self.assert_data_type(&DataType::Bool, &data_type)?;
                self.do_copy(into, mem);
            }
            Expression::BoolLiteral(value) => {
                self.do_add_const(into, value as u8);
            }
            Expression::AsBool(expr) => {

                let (mem, is_temp) = self.evaluate_expression(*expr, &DataType::Byte)?;
                let temp = self.allocate_temporary();

                self.do_move([into.clone(), temp.clone()], mem.clone());
                self.do_move([mem.clone()], into.clone());

                self.do_begin_loop();
                self.do_add_const(into, 1);
                self.do_clear(temp.clone());
                self.do_end_loop(temp.clone(), true);
                
                self.do_free(temp.identifier);
                self.try_free_access(mem, is_temp);

            }
            Expression::Not(expr) => {

                let temp = self.allocate_temporary();
                self.evaluate_bool_expression_into(*expr, temp.clone())?;

                self.do_add_const(into.clone(), 1);

                self.do_begin_loop();
                self.do_sub_const(temp.clone(), 1);
                self.do_sub_const(into, 1);
                self.do_end_loop(temp.clone(), true);

                self.do_free(temp.identifier);

            }
            Expression::And(expr1, expr2) => {

                let (mem1, is_temp1) = self.evaluate_expression(*expr1, &DataType::Bool)?;
                let (mem2, is_temp2) = self.evaluate_expression(*expr2, &DataType::Bool)?;

                let temp = self.allocate_temporary();

                self.do_copy(temp.clone(), mem2.clone());

                self.do_begin_loop();
                self.do_copy(into, mem1.clone());
                self.do_sub_const(temp.clone(), 1);
                self.do_end_loop(temp.clone(), true);

                self.do_free(temp.identifier);
                self.try_free_access(mem2, is_temp2);
                self.try_free_access(mem1, is_temp1);

            }
            Expression::Or(expr1, expr2) => {

                let (mem1, is_temp1) = self.evaluate_expression(*expr1, &DataType::Bool)?;
                let (mem2, is_temp2) = self.evaluate_expression(*expr2, &DataType::Bool)?;

                let temp = self.allocate_temporary();
                self.do_copy(temp.clone(), mem1.clone());
                self.do_copy(temp.clone(), mem2.clone());

                self.do_begin_loop();
                self.do_add_const(into, 1);
                self.do_clear(temp.clone());
                self.do_end_loop(temp.clone(), true);

                self.do_free(temp.identifier);
                self.try_free_access(mem2, is_temp2);
                self.try_free_access(mem1, is_temp1);

            }
            _ => err!(self.current_line_num, IRError::ExpectedTypedExpression(DataType::Bool))
        };

        Ok(())

    }

    fn evaluate_byte_expression_into(&mut self, expression: Expression, into: MemoryAccess, negate: bool) -> Result<(), BrainFricError> {

        match expression {

            Expression::Access(accessor) => {
                
                let (mem, data_type) = self.resolve_accessor(accessor)?;
                self.assert_data_type(&DataType::Byte, &data_type)?;

                if negate {
                    self.do_copy_negative(into, mem);
                }
                else {
                    self.do_copy(into, mem);
                }
            }
            Expression::AsNum(expr) => {
                self.evaluate_bool_expression_into(*expr, into)?;
            }
            Expression::NumberLiteral(value) => {
                if negate {
                    self.do_sub_const(into, value as u8);
                }
                else {
                    self.do_add_const(into, value as u8);
                }
            }
            Expression::Add(expr1, expr2) => {
                self.evaluate_byte_expression_into(*expr1, into.clone(), negate)?;
                self.evaluate_byte_expression_into(*expr2, into, negate)?;
            }
            Expression::Subtract(expr1, expr2) => {
                self.evaluate_byte_expression_into(*expr1, into.clone(), negate)?;
                self.evaluate_byte_expression_into(*expr2, into, !negate)?;
            }
            Expression::Multiply(expr1, expr2) => {

                let temp1 = self.allocate_temporary();
                self.evaluate_byte_expression_into(*expr1, temp1.clone(), false)?;

                let temp2 = self.allocate_temporary();
                self.evaluate_byte_expression_into(*expr2, temp2.clone(), false)?;

                let temp3 = self.allocate_temporary();

                self.do_begin_loop();

                if negate {
                    self.do_move_raw([(into, true), (temp3.clone(), false)], temp1.clone());
                }
                else {
                    self.do_move([into, temp3.clone()], temp1.clone());
                }

                self.do_move([temp1.clone()], temp3.clone());

                self.do_sub_const(temp2.clone(), 1);
                self.do_end_loop(temp2.clone(), false);

                self.do_free_access(temp3);
                self.do_free_access(temp2);
                self.do_free_access(temp1);

            }
            _ => err!(self.current_line_num, IRError::ExpectedTypedExpression(DataType::Byte))
        };

        Ok(())

    }

    fn evaluate_expression_into(&mut self, expression: Expression, expected_type: &DataType, into: MemoryAccess) -> Result<(), BrainFricError> {

        match expected_type {
            DataType::Bool =>
                self.evaluate_bool_expression_into(expression, into),
            DataType::Byte =>
                self.evaluate_byte_expression_into(expression, into, false),
            _ => todo!()
        }
    }

    fn evaluate_expression(&mut self, expression: Expression, expected_type: &DataType) -> Result<(MemoryAccess, bool), BrainFricError> {
        
        Ok(match expression {
            Expression::Access(accessor) => {
                let (mem, data_type) = self.resolve_accessor(accessor)?;
                self.assert_data_type(expected_type, &data_type)?;
                (mem, false)
            }
            _ => {
                let mem = self.allocate_temporary();
                self.evaluate_expression_into(expression, expected_type, mem.clone())?;
                (mem, true)
            }
        })
    }
    
    pub fn generate_ir(&mut self, mut statements: Vec<Statement>) -> Result<IRBlock, BrainFricError> {

        statements.reverse();
        
        while let Some(statement) = statements.pop() {

            self.current_line_num = statement.line_num;
            
            match statement.body {

                StatementBody::Declaration(names, data_type) => {
                    for name in names {
                        self.allocate_variable(name, data_type.clone());
                    }
                }
                StatementBody::SetTo(accessor, expression) => {
                    let (mem, data_type) = self.resolve_accessor(accessor)?;
                    self.assert_can_write(&mem)?;
                    self.do_clear(mem.clone());
                    self.evaluate_expression_into(expression, &data_type, mem)?;

                }
                StatementBody::Inc(accessor) => {
                    let (mem, data_type) = self.resolve_accessor(accessor)?;
                    self.assert_can_modify(&mem)?;
                    self.assert_data_type(&data_type, &DataType::Byte)?;
                    self.do_add_const(mem, 1);

                }
                StatementBody::Dec(accessor) => {
                    let (mem, data_type) = self.resolve_accessor(accessor)?;
                    self.assert_can_modify(&mem)?;
                    self.assert_data_type(&data_type, &DataType::Byte)?;
                    self.do_sub_const(mem, 1);
                }
                StatementBody::Write(expression) => {

                    if let Expression::StringLiteral(literal) = expression {

                        let temp = self.allocate_temporary();

                        let mut val = 0u8;

                        for chr in literal.chars() {
                            self.do_add_const(temp.clone(), (chr as u8).wrapping_sub(val));
                            self.do_write(temp.clone());
                            val = chr as u8;
                        }

                        self.do_free_access(temp);
                        continue;
                    
                    }

                    let (mem, is_temp) = self.evaluate_expression(expression, &DataType::Byte)?;
                    self.do_write(mem.clone());
                    self.try_free_access(mem, is_temp);

                }
                StatementBody::Read(accessor) => {
                    let (access, data_type) = self.resolve_accessor(accessor)?;
                    self.assert_can_write(&access)?;
                    self.assert_data_type(&data_type, &DataType::Byte)?;
                    self.do_read(access);
                }
                StatementBody::While(expression, loop_statements) => {

                    // optimization for special case
                    // also add for bool variable??
                    if let Expression::AsBool(box Expression::Access(accessor)) = expression {

                        let (mem, data_type) = self.resolve_accessor(accessor)?;
                        self.assert_data_type(&data_type, &DataType::Bool)?;
                        
                        self.do_begin_loop();
                        let loop_ir = self.generate_ir(loop_statements)?;
                        self.ir.0.extend(loop_ir.0); 
                        self.do_end_loop(mem, false);

                        continue;

                    }

                    let (mem1, is_temp1) = self.evaluate_expression(expression.clone(), &DataType::Bool)?;

                    self.do_begin_loop();

                    let loop_ir = self.generate_ir(loop_statements)?;
                    self.ir.0.extend(loop_ir.0);

                    let (mem2, is_temp2) = self.evaluate_expression(expression, &DataType::Bool)?;

                    if mem1 != mem2 {
                        self.do_move([mem1.clone()], mem2.clone());
                    }

                    self.do_end_loop(mem1.clone(), false);

                    self.try_free_access(mem2, is_temp2);
                    self.try_free_access(mem1, is_temp1);

                }
                StatementBody::If(expression, loop_statements) => {

                    let temp = self.allocate_temporary();
                    self.evaluate_bool_expression_into(expression, temp.clone())?;
                    
                    self.do_begin_loop();

                    let loop_ir = self.generate_ir(loop_statements)?;
                    self.ir.0.extend(loop_ir.0);

                    self.do_clear(temp.clone());
                    self.do_end_loop(temp.clone(), true);

                    self.do_free(temp.identifier);

                }
            }
        }

        Ok(take(&mut self.ir))
        
    }
}