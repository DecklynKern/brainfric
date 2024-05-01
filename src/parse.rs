use std::ops::Deref;
use std::rc::Rc;
use std::iter::Peekable;
use std::slice::Iter;

use crate::error::*;
use crate::err;
use crate::lex::*;

macro_rules! expect_token {
    ($tokens: expr, $token: pat) => {
        expect_token!($tokens, $token, {})
    };
    
    ($tokens: expr, $token: pat, $if_token: block) => {
        if let Some($token) = $tokens.next() $if_token
        else {
            return None;
        }
    };
}

#[derive(Debug)]
pub enum Definition {
    Enum(Name, Vec<(Name, u8)>)
}

#[derive(Debug)]
pub enum DataTypeHead {
    Bool,
    Byte,
    Short,
    Sequence,
    String,
    Stack,
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

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Specifier {
    ConstIndex(u32),
    Lower,
    Upper
}

#[derive(PartialEq, Eq, Clone)]
pub struct Accessor {
    pub name: Name,
    pub specifiers: Vec<Specifier>
}

impl std::fmt::Debug for Accessor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Accessor(\"{}\", {:?})", self.name, self.specifiers)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expression {

    Access(Accessor),
    EnumItem(Name, Name),
    BoolLiteral(bool),
    NumberLiteral(i32),
    StringLiteral(Rc<str>),

    Equals(Box<Expression>, Box<Expression>),
    NotEquals(Box<Expression>, Box<Expression>),
    LessThan(Box<Expression>, Box<Expression>),
    GreaterThan(Box<Expression>, Box<Expression>),
    LessThanEqual(Box<Expression>, Box<Expression>),
    GreaterThanEqual(Box<Expression>, Box<Expression>),

    Not(Box<Expression>),
    And(Box<Expression>, Box<Expression>),
    Or(Box<Expression>, Box<Expression>),

    Add(Box<Expression>, Box<Expression>),
    Subtract(Box<Expression>, Box<Expression>),
    Multiply(Box<Expression>, Box<Expression>),
    Divide(Box<Expression>, Box<Expression>),

    AsBool(Box<Expression>),
    AsNum(Box<Expression>)

}

#[derive(Debug)]
pub enum StatementBody {
    Declaration(Vec<Name>, ParsedDataType),
    SetTo(Accessor, Expression),
    Inc(Accessor),
    Dec(Accessor),
    Clear(Accessor),
    LeftShift(Accessor, u32),
    RightShift(Accessor, u32),
    Write(Expression),
    WriteNum(Expression),
    WriteLine,
    Read(Accessor),
    While(Expression, Block),
    If(Expression, Block, Option<Block>),
    Switch(Expression, Vec<(u8, Block)>, Option<Block>)
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
                    found_newline = true;

                }
                _ => break
            }
        }

        if found_newline {
            Ok(())
        }
        else {
            err!(self.line_num, ParseError::ExpectedNewline);
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

        let Some(token) = self.tokens.peek() else {
            err!(self.line_num, ParseError::InvalidType);
        };

        if let Token::NumberLiteral(num) = token {
            self.tokens.next();
            Ok(DataTypeParameter::Constant(*num as usize))
        }
        else {
            self.parse_data_type().map(DataTypeParameter::Type)
        }
    }

    fn parse_data_type(&mut self) -> Result<ParsedDataType, BrainFricError> {

        let Some(token) = self.tokens.peek() else {
            err!(self.line_num, ParseError::InvalidType);
        };

        let head = match token {
            Token::Bool => DataTypeHead::Bool,
            Token::Byte => DataTypeHead::Byte,
            Token::Short => DataTypeHead::Short,
            Token::Sequence => DataTypeHead::Sequence,
            Token::String => DataTypeHead::String,
            Token::Stack => todo!(),
            Token::Array => todo!(),
            Token::Identifier(name) => DataTypeHead::UserDefined(name.clone()),
            _ => err!(self.line_num, ParseError::InvalidType)
        };

        let mut data_type = ParsedDataType {
            head,
            parameters: Vec::new()
        };

        self.tokens.next();

        if self.try_take_token(Token::OpenAngle) {

            let mut first_param = true;

            loop {

                if self.try_take_token(Token::CloseAngle) {
                    return Ok(data_type);
                }

                data_type.parameters.push(self.parse_data_type_parameter()?);

                if !first_param && !self.try_take_token(Token::Comma) {
                    err!(self.line_num, ParseError::InvalidType);
                }

                first_param = false;

            }
        }
        else {
            Ok(data_type)
        }
    }

    fn try_parse_specifier(&mut self) -> Option<Specifier> {

        if self.tokens.peek().is_some_and(|token| token.is_specifier_head()) {

            Some(match self.tokens.next().unwrap() {
                Token::At => 
                    // change when array happens
                    expect_token!(self.tokens, Token::NumberLiteral(idx), {Specifier::ConstIndex(*idx as u32)}),
                Token::Dot => match self.tokens.next() {
                    Some(Token::Identifier(ident)) => match ident.deref() {
                        "low" => Specifier::Lower,
                        "high" => Specifier::Upper,
                        _ => return None
                    }
                    _ => return None
                }
                _ => unreachable!()
            })
        }
        else {
            None
        }
    }

    fn parse_specifiers(&mut self) -> Vec<Specifier> {

        let mut specifiers = Vec::new();

        while let Some(specifier) = self.try_parse_specifier() {
            specifiers.push(specifier);
        }

        specifiers

    }

    fn parse_accessor(&mut self) -> Result<Accessor, BrainFricError> {

        let Some(Token::Identifier(name)) = self.tokens.next()
        else {
            err!(self.line_num, ParseError::InvalidExpression);
        };

        Ok(Accessor {
            name: name.clone(),
            specifiers: self.parse_specifiers()
        })
    }

    fn try_parse_binary_operator_precedence_2(&mut self) -> Option<fn(Box<Expression>, Box<Expression>) -> Expression> {

        let Some(op_token) = self.tokens.peek() else {
            return None;
        };

        let op_func = match op_token {
            Token::Star => Expression::Multiply,
            Token::ForwardSlash => Expression::Divide,
            _ => return None
        };

        self.tokens.next();
        Some(op_func)

    }

    fn try_parse_binary_operator_precedence_1(&mut self) -> Option<fn(Box<Expression>, Box<Expression>) -> Expression> {

        let Some(op_token) = self.tokens.peek() else {
            return None;
        };

        let op_func = match op_token {
            Token::Equal => Expression::Equals,
            Token::NotEqual => Expression::NotEquals,
            Token::OpenAngle => Expression::LessThan,
            Token::CloseAngle => Expression::GreaterThan,
            Token::LessThanEqual => Expression::LessThanEqual,
            Token::GreaterThanEqual => Expression::GreaterThanEqual,
            Token::Plus => Expression::Add,
            Token::Hypen => Expression::Subtract,
            Token::Ampersand => Expression::And,
            Token::Pipe => Expression::Or,
            _ => return None
        };

        self.tokens.next();
        Some(op_func)

    }

    fn parse_factor(&mut self) -> Result<Expression, BrainFricError> {

        let Some(token) = self.tokens.next() else {
            err!(self.line_num, ParseError::InvalidExpression);
        };

        Ok(match token {
            Token::OpenParen => {

                let expression = self.parse_expression()?;

                if !self.try_take_token(Token::CloseParen) {
                    err!(self.line_num, ParseError::ExpectedCloseParen);
                }

                expression
                
            }
            Token::BoolLiteral(val) => Expression::BoolLiteral(*val),
            Token::NumberLiteral(val) => Expression::NumberLiteral(*val),
            Token::CharLiteral(chr) => Expression::NumberLiteral(*chr as i32),
            Token::StringLiteral(val) => Expression::StringLiteral(val.clone()),
            Token::Identifier(name) => {

                if self.try_take_token(Token::DoubleColon) {

                    let Some(Token::Identifier(enum_variant)) = self.tokens.next() else {
                        err!(self.line_num, ParseError::ExpectedName);
                    };

                    Expression::EnumItem(name.clone(), enum_variant.clone())

                }
                else {
                    Expression::Access(Accessor {
                        name: name.clone(),
                        specifiers: self.parse_specifiers()
                    })
                }
            }
            _ => err!(self.line_num, ParseError::InvalidExpression)
        })
    }

    fn parse_term(&mut self) -> Result<Expression, BrainFricError> {

        let Some(token) = self.tokens.peek()
        else {
            err!(self.line_num, ParseError::InvalidExpression);
        };

        if token.is_unary_operator() {

            let op = self.tokens.next().unwrap();
            let factor = self.parse_factor()?;

            Ok(match op {
                Token::Question => Expression::AsBool,
                Token::Pound => Expression::AsNum,
                Token::Exclamation => Expression::Not,
                _ => unreachable!()
            }(Box::new(factor)))
        }
        else {

            let factor1 = self.parse_factor()?;

            let Some(operator) = self.try_parse_binary_operator_precedence_2()
            else {
                return Ok(factor1);
            };

            let factor2 = self.parse_factor()?;

            let mut expr = operator(Box::new(factor1), Box::new(factor2));

            while let Some(next_operator) = self.try_parse_binary_operator_precedence_2() {
                let next_term = self.parse_factor()?;
                expr = next_operator(Box::new(expr), Box::new(next_term));
            }

            Ok(expr)

        }
    }

    fn parse_expression(&mut self) -> Result<Expression, BrainFricError> {

        let term1 = self.parse_term()?;

        let Some(operator) = self.try_parse_binary_operator_precedence_1()
        else {
            return Ok(term1);
        };

        let term2 = self.parse_term()?;

        let mut expr = operator(Box::new(term1), Box::new(term2));

        while let Some(next_operator) = self.try_parse_binary_operator_precedence_1() {
            let next_term = self.parse_term()?;
            expr = next_operator(Box::new(expr), Box::new(next_term));
        }

        Ok(expr)

    }

    fn parse_statement(&mut self) -> Result<Statement, BrainFricError> {

        let line_num = self.line_num;

        let Some(token) = self.tokens.next() else {
            todo!();
        };

        let body = match token {
            Token::Var => {
                
                let Some(Token::Identifier(name1)) = self.tokens.next()
                else {
                    err!(self.line_num, ParseError::InvalidStatement);
                };

                let mut names = vec![name1.clone()];

                while self.try_take_token(Token::Comma) {
                    
                    let Some(Token::Identifier(name)) = self.tokens.next()
                    else {
                        err!(self.line_num, ParseError::InvalidStatement)
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
                            err!(self.line_num, ParseError::MultipleDefaultArms);
                        }

                        self.expect_newline()?;
                        default = Some(self.parse_block()?);

                        continue;

                    }

                    if !self.try_take_token(Token::Case) {
                        break;
                    }

                    let val = match self.tokens.next() {
                        Some(Token::NumberLiteral(num)) => *num as u8,
                        Some(Token::CharLiteral(chr)) => *chr as u8,
                        _ => err!(self.line_num, ParseError::ExpectedNumberLiteral)
                    };

                    self.expect_newline()?;

                    let block = self.parse_block()?;

                    arms.push((val, block));

                }

                if !self.try_take_token(Token::End) {
                    err!(self.line_num, ParseError::ExpectedEnd);
                }

                StatementBody::Switch(expr, arms, default)

            }
            Token::Identifier(name) => {

                let accessor = Accessor {
                    name: name.clone(),
                    specifiers: self.parse_specifiers()
                };

                self.try_take_token(Token::SetTo);
                let expression = self.parse_expression()?;

                StatementBody::SetTo(accessor, expression)

            }
            Token::Inc => StatementBody::Inc(self.parse_accessor()?),
            Token::Dec => StatementBody::Dec(self.parse_accessor()?),
            Token::Clear => StatementBody::Clear(self.parse_accessor()?),
            Token::Read => StatementBody::Read(self.parse_accessor()?),
            Token::LeftShift | Token::RightShift => {
                
                if !self.try_take_token(Token::OpenAngle) {
                    err!(self.line_num, ParseError::ExpectedOpenAngle);
                }
                
                let Some(Token::NumberLiteral(offset)) = self.tokens.next()
                else {
                    err!(self.line_num, ParseError::ExpectedNumberLiteral);
                };
                
                if !self.try_take_token(Token::CloseAngle) {
                    err!(self.line_num, ParseError::ExpectedCloseAngle);
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
            Token::WriteNum => StatementBody::WriteNum(self.parse_expression()?),
            Token::WriteLine => StatementBody::WriteLine,
            _ => err!(self.line_num, ParseError::InvalidStatement)
        };
        
        Ok(Statement {
            line_num,
            body
        })
    }

    fn parse_block(&mut self) -> Result<Block, BrainFricError> {

        let mut statements = Vec::new();

        while let Some(&token) = self.tokens.peek() {

            if *token == Token::End {
                self.tokens.next();
                break;
            }

            // hacky
            if *token == Token::Else {
                break;
            }

            statements.push(self.parse_statement()?);

            if !self.tokens.is_empty() {
                self.expect_newline()?;
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
                    err!(self.line_num, ParseError::ExpectedName);
                };

                self.expect_newline()?;

                let mut next_value = 0;
                let mut variants = Vec::new();

                while let Some(token) = self.tokens.next() {
                    match token {
                        Token::End => {
                            break;
                        }
                        Token::Identifier(variant_name) => {
                            
                            let value = if self.try_take_token(Token::Equal) {

                                let Some(Token::NumberLiteral(val)) = self.tokens.next() else {
                                    err!(self.line_num, ParseError::InvalidDefinition);
                                };

                                *val as u8

                            }
                            else {
                                next_value
                            };

                            next_value = value + 1;

                            variants.push((variant_name.clone(), value))

                        }
                        _ => err!(self.line_num, ParseError::InvalidDefinition)
                    }

                    self.expect_newline()?;

                }

                Definition::Enum(name.clone(), variants)

            }
            _ => err!(self.line_num, ParseError::InvalidDefinition)
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