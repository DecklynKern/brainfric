use std::mem::replace;
use std::rc::Rc;
use std::iter::Peekable;
use std::slice::Iter;

use crate::error::*;
use crate::err;
use crate::lex::*;

#[derive(Debug)]
pub enum Definition {
    Enum(Name, Vec<(Name, u8)>),
    Struct(Name, Vec<(Name, ParsedDataType)>),
    Macro(Name, Vec<Name>, Expression)
}

#[derive(Debug)]
pub enum DataTypeHead {
    Bool,
    Byte,
    Short,
    Fixed,
    Sequence,
    Stack,
    Array,
    UserDefined(Name)
}

#[derive(Debug)]
pub enum DataTypeParameter {
    Constant(usize),
    Type(ParsedDataType)
}

impl DataTypeParameter {
    pub fn get_type_val(&self) -> u32 {
        match self {
            Self::Constant(_) => 0,
            Self::Type(_) => 1
        }
    }
}

#[derive(Debug)]
pub struct ParsedDataType {
    pub head: DataTypeHead,
    pub parameters: Vec<DataTypeParameter>
}

impl Default for ParsedDataType {
    fn default() -> Self {
        Self {
            head: DataTypeHead::Byte,
            parameters: Vec::new()
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Specifier {
    Index(Expression),
    Field(Name),
    StackPop
}

#[derive(PartialEq, Eq, Clone)]
pub struct Accessor {
    pub name: Name,
    pub specifiers: Box<[Specifier]>
}

impl std::fmt::Debug for Accessor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Accessor(\"{}\", {:?})", self.name, self.specifiers)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum MatchArm {
    NumberLiteral(u8),
    EnumVariant(Name, Name)
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum UnaryOperator {
    Not,
    AsBool,
    AsByte
}

impl UnaryOperator {
    pub fn try_parse(token: &Token) -> Option<Self> {
        Some(match token {
            Token::Exclamation => Self::Not,
            Token::Question => Self::AsBool,
            Token::Octothorpe => Self::AsByte,
            _ => return None
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BinaryOperator {
    Equals,
    NotEquals,
    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,
    And,
    Or,
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo
}

impl BinaryOperator {

    pub fn try_parse_precedence_1(token: &Token) -> Option<Self> {
        Some(match token {
            Token::Equal => Self::Equals,
            Token::NotEqual => Self::NotEquals,
            Token::OpenAngle => Self::LessThan,
            Token::CloseAngle => Self::GreaterThan,
            Token::LessThanEqual => Self::LessThanEqual,
            Token::GreaterThanEqual => Self::GreaterThanEqual,
            Token::Plus => Self::Add,
            Token::Hypen => Self::Subtract,
            Token::Ampersand => Self::And,
            Token::Pipe => Self::Or,
            _ => return None
        })
    }
    
    pub fn try_parse_precedence_2(token: &Token) -> Option<Self> {
        Some(match token {
            Token::Star => Self::Multiply,
            Token::ForwardSlash => Self::Divide,
            Token::Percent => Self::Modulo,
            _ => return None
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expression {

    Access(Accessor),
    EnumVariant(Name, Name),
    BoolLiteral(bool),
    NumberLiteral(i32),
    StringLiteral(Rc<str>),

    UnaryExpression(UnaryOperator, Box<Expression>),
    BinaryExpression(BinaryOperator, Box<Expression>, Box<Expression>),

    MacroInvocation(Name, Box<[Expression]>)

}

impl Expression {

    pub fn replace(&mut self, parameters: &[Name], expressions: &[Expression]) {

        let new_expr_idx = match self {
            Self::Access(accessor) => {

                if accessor.specifiers.is_empty() {

                    let Some(idx) = parameters.iter().position(|param| *param == accessor.name)
                    else {
                        return;
                    };

                    idx

                }
                else {
                    return;
                }
            }
            Self::UnaryExpression(_, child) => {
                child.replace(parameters, expressions);
                return;
            }
            Self::BinaryExpression(_, child1, child2) => {
                child1.replace(parameters, expressions);
                child2.replace(parameters, expressions);
                return;
            }
            _ => return
        };

        let _ = replace(self, expressions[new_expr_idx].clone());

    }
}

#[derive(Debug)]
pub enum StatementBody {
    Declaration(Vec<Name>, ParsedDataType),
    Assign(Accessor, Expression),
    Inc(Accessor),
    Dec(Accessor),
    Clear(Accessor),
    LeftShift(Accessor, u32),
    RightShift(Accessor, u32),
    Write(Expression),
    WriteAsNum(Expression),
    WriteLine,
    Read(Accessor),
    While(Expression, Block),
    If(Expression, Block, Option<Block>),
    Switch(Expression, Box<[(MatchArm, Block)]>, Option<Block>)
}

pub struct Statement {
    pub line_num: usize,
    pub body: StatementBody
}

impl std::fmt::Debug for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {:#?}", self.line_num, self.body)
    }
}

pub type Block = Vec<Statement>;

pub struct ParsedProgram {
    pub definitions: Vec<Definition>,
    pub code: Block
}

struct Parser<'a> {
    line_num: usize,
    tokens: Peekable<Iter<'a, Token>>
}

impl<'a> Parser<'a> {

    fn expect_newline(&mut self) -> Result<(), BrainFricError> {

        let mut found_newline = false;

        loop {
            match self.tokens.peek() {
                Some(Token::Comment) => {
                    self.tokens.next();
                }
                Some(Token::Newline) => {

                    self.tokens.next();

                    self.line_num += 1;
                    set_line_num(self.line_num);
                    found_newline = true;

                }
                _ => break
            }
        }

        if found_newline {
            Ok(())
        }
        else {
            err!(ParseError::ExpectedNewline);
        }
    }

    fn try_take_token(&mut self, token: Token) -> bool {
        
        if self.tokens.is_empty() {
            false
        }
        else if **self.tokens.peek().unwrap() == token {
            self.tokens.next();
            true
        }
        else {
            false
        }
    }

    fn parse_data_type_parameter(&mut self) -> Result<DataTypeParameter, BrainFricError> {

        let Some(token) = self.tokens.peek()
        else {
            err!(ParseError::InvalidType);
        };

        if let Token::IntegerLiteral(num) = token {
            self.tokens.next();
            Ok(DataTypeParameter::Constant(*num as usize))
        }
        else {
            self.parse_data_type().map(DataTypeParameter::Type)
        }
    }

    fn parse_data_type(&mut self) -> Result<ParsedDataType, BrainFricError> {

        let Some(token) = self.tokens.peek()
        else {
            err!(ParseError::InvalidType);
        };

        let head = match token {
            Token::Bool => DataTypeHead::Bool,
            Token::Byte => DataTypeHead::Byte,
            Token::Short => DataTypeHead::Short,
            Token::Fixed => DataTypeHead::Fixed,
            Token::Sequence => DataTypeHead::Sequence,
            Token::Stack => DataTypeHead::Stack,
            Token::Array => DataTypeHead::Array,
            Token::Identifier(name) => DataTypeHead::UserDefined(name.clone()),
            _ => err!(ParseError::InvalidType)
        };

        let mut data_type = ParsedDataType {
            head,
            parameters: Vec::new()
        };

        if let DataTypeHead::UserDefined(_) = data_type.head {
            return Ok(data_type);
        }

        self.tokens.next();

        if self.try_take_token(Token::OpenAngle) {

            let mut first_param = true;

            loop {

                if self.try_take_token(Token::CloseAngle) {
                    return Ok(data_type);
                }

                if !first_param && !self.try_take_token(Token::Comma) {
                    err!(ParseError::InvalidType);
                }

                data_type.parameters.push(self.parse_data_type_parameter()?);

                first_param = false;

            }
        }
        else {
            Ok(data_type)
        }
    }

    fn try_parse_specifier(&mut self) -> Result<Option<Specifier>, BrainFricError> {

        if self.tokens.peek().is_some_and(|token| token.is_specifier_head()) {

            Ok(Some(match self.tokens.next().unwrap() {
                Token::OpenSquare => {

                    let expr = self.parse_expression()?;

                    if !self.try_take_token(Token::CloseSquare) {
                        err!(ParseError::ExpectedCloseSquare)
                    }

                    println!("{expr:?}");

                    Specifier::Index(expr)
                    
                }
                Token::Dot => match self.tokens.next() {
                    Some(Token::Identifier(ident)) => Specifier::Field(ident.clone()),
                    _ => return Ok(None)
                }
                Token::Exclamation => Specifier::StackPop,
                _ => unreachable!()
            }))
        }
        else {
            Ok(None)
        }
    }

    fn parse_specifiers(&mut self) -> Result<Box<[Specifier]>, BrainFricError> {

        let mut specifiers = Vec::new();

        while let Some(specifier) = self.try_parse_specifier()? {
            specifiers.push(specifier);
        }

        Ok(specifiers.into_boxed_slice())

    }

    fn parse_accessor(&mut self) -> Result<Accessor, BrainFricError> {

        let Some(Token::Identifier(name)) = self.tokens.next()
        else {
            err!(ParseError::InvalidExpression);
        };

        Ok(Accessor {
            name: name.clone(),
            specifiers: self.parse_specifiers()?
        })
    }

    fn parse_factor(&mut self) -> Result<Expression, BrainFricError> {

        let Some(token) = self.tokens.next()
        else {
            err!(ParseError::InvalidExpression);
        };

        Ok(match token {
            Token::OpenParen => {

                let expression = self.parse_expression()?;

                if !self.try_take_token(Token::CloseParen) {
                    err!(ParseError::ExpectedCloseParen);
                }

                expression
                
            }
            Token::BoolLiteral(val) => Expression::BoolLiteral(*val),
            Token::IntegerLiteral(val) => Expression::NumberLiteral(*val),
            Token::CharLiteral(chr) => Expression::NumberLiteral(*chr as i32),
            Token::StringLiteral(val) => Expression::StringLiteral(val.clone()),
            Token::Identifier(name) => {

                if self.try_take_token(Token::DoubleColon) {

                    let Some(Token::Identifier(enum_variant)) = self.tokens.next() else {
                        err!(ParseError::ExpectedName);
                    };

                    Expression::EnumVariant(name.clone(), enum_variant.clone())

                }
                else {
                    Expression::Access(Accessor {
                        name: name.clone(),
                        specifiers: self.parse_specifiers()?
                    })
                }
            }
            Token::Dollar => {

                let Some(Token::Identifier(name)) = self.tokens.next()
                else {
                    err!(ParseError::ExpectedIdentifier);
                };

                let mut arguments = Vec::new();

                if self.try_take_token(Token::OpenParen) {

                    arguments.push(self.parse_expression()?);

                    while self.try_take_token(Token::Comma) {
                        arguments.push(self.parse_expression()?);
                    }

                    if !self.try_take_token(Token::CloseParen) {
                        err!(ParseError::ExpectedCloseParen);
                    }
                }

                Expression::MacroInvocation(name.clone(), arguments.into_boxed_slice())

            }
            _ => err!(ParseError::InvalidExpression)
        })
    }

    fn parse_term(&mut self) -> Result<Expression, BrainFricError> {

        let Some(token) = self.tokens.peek().cloned()
        else {
            err!(ParseError::InvalidExpression);
        };

        if let Some(operator) = UnaryOperator::try_parse(&token) {

            self.tokens.next();

            let factor = self.parse_factor()?;

            Ok(Expression::UnaryExpression(operator, Box::new(factor)))
            
        }
        else {

            let factor1 = self.parse_factor()?;

            let Some(operator) = self.tokens.peek().cloned().and_then(BinaryOperator::try_parse_precedence_2)
            else {
                return Ok(factor1);
            };

            self.tokens.next();

            let factor2 = self.parse_factor()?;

            let mut expr = Expression::BinaryExpression(operator, Box::new(factor1), Box::new(factor2));

            while let Some(next_operator) = self.tokens.peek().cloned().and_then(BinaryOperator::try_parse_precedence_2) {

                self.tokens.next();

                let next_factor = self.parse_factor()?;
                expr = Expression::BinaryExpression(next_operator, Box::new(expr), Box::new(next_factor));

            }

            Ok(expr)

        }
    }

    // likely merge with term code
    fn parse_expression(&mut self) -> Result<Expression, BrainFricError> {

        let term1 = self.parse_term()?;

        let Some(operator) = self.tokens.peek().cloned().and_then(BinaryOperator::try_parse_precedence_1)
        else {
            return Ok(term1);
        };

        self.tokens.next();

        let term2 = self.parse_term()?;

        let mut expr = Expression::BinaryExpression(operator, Box::new(term1), Box::new(term2));

        while let Some(next_operator) = self.tokens.peek().cloned().and_then(BinaryOperator::try_parse_precedence_1) {

            self.tokens.next();

            let next_term = self.parse_term()?;
            expr = Expression::BinaryExpression(next_operator, Box::new(expr), Box::new(next_term));
            
        }

        Ok(expr)

    }

    fn parse_statement(&mut self) -> Result<Statement, BrainFricError> {

        let line_num = self.line_num;

        let Some(token) = self.tokens.next()
        else {
            todo!();
        };

        let body = match token {
            Token::Var => {
                
                let Some(Token::Identifier(name1)) = self.tokens.next()
                else {
                    err!(ParseError::InvalidStatement);
                };

                let mut names = vec![name1.clone()];

                while self.try_take_token(Token::Comma) {
                    
                    let Some(Token::Identifier(name)) = self.tokens.next()
                    else {
                        err!(ParseError::InvalidStatement)
                    };

                    names.push(name.clone());

                }

                let data_type = if self.try_take_token(Token::Colon) {
                    self.parse_data_type()?
                }
                else {
                    ParsedDataType::default()
                };
    
                StatementBody::Declaration(names, data_type)

            }
            Token::If => {

                let condition = self.parse_expression()?;
                self.expect_newline()?;

                let block = self.parse_block()?;

                let else_block = if self.try_take_token(Token::Else) {
                    self.expect_newline()?;
                    Some(self.parse_block()?)
                }
                else {
                    None
                };

                StatementBody::If(condition, block, else_block)

            }
            Token::While => {

                let condition = self.parse_expression()?;
                self.expect_newline()?;

                let block = self.parse_block()?;

                StatementBody::While(condition, block)

            }
            Token::Switch => {

                let expr = self.parse_expression()?;

                let mut arms = Vec::new();
                let mut default = None;

                loop {
                    
                    self.expect_newline()?;

                    if self.try_take_token(Token::Default) {

                        if default.is_some() {
                            err!(ParseError::MultipleDefaultArms);
                        }

                        self.expect_newline()?;
                        default = Some(self.parse_block()?);

                        continue;

                    }

                    if !self.try_take_token(Token::Case) {
                        break;
                    }

                    let arm = match self.parse_factor()? {
                        Expression::NumberLiteral(num) => MatchArm::NumberLiteral(num as u8),
                        Expression::EnumVariant(enum_name, variant_name) => MatchArm::EnumVariant(enum_name, variant_name),
                        _ => err!(ParseError::ExpectedMatchArm)
                    };

                    self.expect_newline()?;

                    let block = self.parse_block()?;

                    arms.push((arm, block));

                }

                if !self.try_take_token(Token::End) {
                    err!(ParseError::ExpectedEnd);
                }

                StatementBody::Switch(expr, arms.into_boxed_slice(), default)

            }
            Token::Identifier(name) => {

                let accessor = Accessor {
                    name: name.clone(),
                    specifiers: self.parse_specifiers()?
                };

                self.try_take_token(Token::SetTo);
                let expression = self.parse_expression()?;

                StatementBody::Assign(accessor, expression)

            }
            Token::Inc => StatementBody::Inc(self.parse_accessor()?),
            Token::Dec => StatementBody::Dec(self.parse_accessor()?),
            Token::Clear => StatementBody::Clear(self.parse_accessor()?),
            Token::Read => StatementBody::Read(self.parse_accessor()?),
            Token::LeftShift | Token::RightShift => {
                
                if !self.try_take_token(Token::OpenAngle) {
                    err!(ParseError::ExpectedOpenAngle);
                }
                
                let Some(Token::IntegerLiteral(offset)) = self.tokens.next()
                else {
                    err!(ParseError::ExpectedNumberLiteral);
                };
                
                if !self.try_take_token(Token::CloseAngle) {
                    err!(ParseError::ExpectedCloseAngle);
                }
                
                let accessor = self.parse_accessor()?;
                
                if *token == Token::LeftShift {
                    StatementBody::LeftShift(accessor, *offset as u32)
                }
                else {
                    StatementBody::RightShift(accessor, *offset as u32)
                }
            }
            Token::Write => StatementBody::Write(self.parse_expression()?),
            Token::WriteNum => StatementBody::WriteAsNum(self.parse_expression()?),
            Token::WriteLine => StatementBody::WriteLine,
            _ => err!(ParseError::InvalidStatement)
        };
        
        Ok(Statement {
            line_num,
            body
        })
    }

    fn parse_block(&mut self) -> Result<Block, BrainFricError> {

        let mut statements = Vec::new();

        while let Some(&token) = self.tokens.peek() {

            match token {
                Token::Newline => {
                    self.tokens.next();
                    continue;
                }
                Token::End => {
                    self.tokens.next();
                    break;
                }
                // hacky
                Token::Else => {
                    break;
                }
                _ => {

                    statements.push(self.parse_statement()?);

                    if !self.tokens.is_empty() {
                        self.expect_newline()?;
                    }
                }
            }
        }

        Ok(statements)

    }

    fn parse_definition(&mut self) -> Result<Definition, BrainFricError> {

        if self.tokens.is_empty() {
            todo!()
        }

        Ok(match self.tokens.next().unwrap() {
            Token::Enum => {

                let Some(Token::Identifier(name)) = self.tokens.next() else {
                    err!(ParseError::ExpectedName);
                };

                self.expect_newline()?;

                let mut next_value = 0;
                let mut variants = Vec::new();

                while let Some(token) = self.tokens.next() {
                    match token {
                        Token::End => break,
                        Token::Identifier(variant_name) => {
                            
                            let value = if self.try_take_token(Token::Equal) {

                                let Some(Token::IntegerLiteral(val)) = self.tokens.next() else {
                                    err!(ParseError::InvalidDefinition);
                                };

                                *val as u8

                            }
                            else {
                                next_value
                            };

                            next_value = value + 1;

                            variants.push((variant_name.clone(), value))

                        }
                        _ => err!(ParseError::InvalidDefinition)
                    }

                    self.expect_newline()?;

                }

                Definition::Enum(name.clone(), variants)

            }
            Token::Struct => {

                let Some(Token::Identifier(name)) = self.tokens.next() else {
                    err!(ParseError::ExpectedName);
                };

                self.expect_newline()?;

                let mut fields = Vec::new();

                while let Some(token) = self.tokens.next() {
                    match token {
                        Token::End => break,
                        Token::Identifier(field_name) => {

                            let data_type = if self.try_take_token(Token::Colon) {
                                self.parse_data_type()?
                            }
                            else {
                                ParsedDataType::default()
                            };

                            fields.push((field_name.clone(), data_type));

                        }
                        _ => err!(ParseError::InvalidDefinition)
                    }

                    self.expect_newline()?;

                }

                Definition::Struct(name.clone(), fields)

            }
            Token::Identifier(name) => {

                let mut arguments = Vec::new();

                if self.try_take_token(Token::OpenParen) {

                    let Some(Token::Identifier(arg1)) = self.tokens.next()
                    else {
                        err!(ParseError::ExpectedIdentifier);
                    };

                    arguments.push(arg1.clone());

                    while self.try_take_token(Token::Comma) {

                        let Some(Token::Identifier(next_arg)) = self.tokens.next()
                        else {
                            err!(ParseError::ExpectedIdentifier);
                        };

                        arguments.push(next_arg.clone());

                    }

                    if !self.try_take_token(Token::CloseParen) {
                        err!(ParseError::ExpectedCloseParen);
                    }
                }

                if !self.try_take_token(Token::Equal) {
                    err!(ParseError::ExpectedEqual);
                }

                let expression = self.parse_expression()?;

                Definition::Macro(name.clone(), arguments, expression)
                
            }
            _ => err!(ParseError::InvalidDefinition)
        })
    }

    fn parse_program(&mut self) -> Result<ParsedProgram, BrainFricError> {

        let mut definitions = Vec::new();

        if self.try_take_token(Token::Define) {

            self.expect_newline()?;

            while let Some(&token) = self.tokens.peek() {

                if *token == Token::End {
                    self.tokens.next();
                    self.expect_newline()?;
                    break;
                }

                definitions.push(self.parse_definition()?);

                if !self.tokens.is_empty() {
                    self.expect_newline()?;
                }
            }
        };

        let code = self.parse_block()?;

        Ok(ParsedProgram {
            definitions,
            code
        })
    }
}

pub fn parse(code: &[Token]) -> Result<ParsedProgram, BrainFricError> {

    let mut parser = Parser {
        line_num: 1,
        tokens: code.iter().peekable()
    };

    parser.parse_program()

}