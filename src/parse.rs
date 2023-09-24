use std::rc::Rc;
use std::iter::Peekable;
use std::slice::Iter;

use crate::error::*;
use crate::err;
use crate::lex::*;

macro_rules! expect_token {
    ($tokens: ident, $token: expr) => {
        if let Some(token) = $tokens.next() {
            if *token != $token {
                return None;
            }
        }
        else {
            return None;
        }
    };
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DataType {
    Bool,
    Byte,
    Short,
    Stack(usize),
    //Array(Rc<DataType>, usize)
}

impl DataType {

    pub fn get_size(&self) -> usize {

        match &self {
            Self::Bool => 1,
            Self::Byte => 1,
            Self::Short => 2,
            Self::Stack(len) => *len + 2,
            //Self::Array(data_type, len) => data_type.get_size() * len
        }
    }

    fn try_parse(tokens: &mut Peekable<Iter<Token>>) -> Option<Self> {

        tokens.next().and_then(|type_name| Some(match type_name {
            Token::Bool => Self::Bool,
            Token::Byte => Self::Byte,
            Token::Short => Self::Short,
            Token::Stack => {

                expect_token!(tokens, Token::OpenAngle);

                match tokens.next() {
                    Some(Token::NumberLiteral(size)) => {
                        expect_token!(tokens, Token::CloseAngle);
                        DataType::Stack(*size as usize)
                    }
                    _ => return None
                }
            }
            Token::Array => todo!(),
            _ => unreachable!()
        }))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Accessor {
    Identifier(Name),
    Push(Box<Accessor>),
    Pop(Box<Accessor>)
}

impl Accessor {

    fn try_parse(tokens: &mut Peekable<Iter<Token>>) -> Option<Self> {

        tokens.next().and_then(|&token| match token {
            Token::Identifier(name) => Some(Self::Identifier(name.clone())),
            _ => None
        })
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expression {

    Access(Accessor),
    BoolLiteral(bool),
    NumberLiteral(i32),
    StringLiteral(Rc<str>),

    Equals(Box<Expression>, Box<Expression>),
    LessThan(Box<Expression>, Box<Expression>),
    GreaterThan(Box<Expression>, Box<Expression>),

    Not(Box<Expression>),
    And(Box<Expression>, Box<Expression>),
    Or(Box<Expression>, Box<Expression>),

    Add(Box<Expression>, Box<Expression>),
    Subtract(Box<Expression>, Box<Expression>),
    Multiply(Box<Expression>, Box<Expression>),

    AsBool(Box<Expression>),
    AsNum(Box<Expression>)

}

impl Expression {

    fn try_parse_factor(tokens: &mut Peekable<Iter<Token>>) -> Option<Self> {

        tokens.next().and_then(|token| match token {

            Token::OpenParen => Self::try_parse(tokens).and_then(|expr| {
                expect_token!(tokens, Token::CloseParen);
                Some(expr)
            }),
            Token::BoolLiteral(val) => Some(Self::BoolLiteral(*val)),
            Token::NumberLiteral(val) => Some(Self::NumberLiteral(*val)),
            Token::CharLiteral(chr) => Some(Self::NumberLiteral(*chr as i32)),
            Token::StringLiteral(val) => Some(Self::StringLiteral(val.clone())),
            _ => Accessor::try_parse(tokens).map(Self::Access)
        })
    }

    fn try_parse_term(tokens: &mut Peekable<Iter<Token>>) -> Option<Self> {

        match tokens.peek() {

            Some(&token) => {

                if token.is_unary_operator() {

                    let op = token.clone();

                    tokens.next();
    
                    Self::try_parse_term(tokens).map(|term| match op {
                        Token::AsBool => Self::AsBool,
                        Token::AsNum => Self::AsNum,
                        Token::Not => Self::Not,
                        _ => unreachable!()
                    }(Box::new(term)))
                }
                else {
                        
                    Self::try_parse_factor(tokens).and_then(|factor1| match tokens.peek() {

                        Some(Token::Star) => {
                            
                            tokens.next();

                            Self::try_parse_factor(tokens).map(|factor2|
                                Self::Multiply(Box::new(factor1), Box::new(factor2))
                            )
                        }
                        _ => Some(factor1)
                    })
                }
            }
            None => None
        }
    }

    fn try_parse(tokens: &mut Peekable<Iter<Token>>) -> Option<Self> {

        Expression::try_parse_term(tokens).and_then(|term1| match tokens.peek() {

            Some(&token) => {

                if token.is_binary_operator() {

                    let op = token.clone();
    
                    tokens.next();
    
                    Self::try_parse_term(tokens)
                        .and_then(|term2| Some(match op {
                            Token::Equal => Self::Equals,
                            Token::OpenAngle => Self::LessThan,
                            Token::CloseAngle => Self::GreaterThan,
                            Token::Plus => Self::Add,
                            Token::Hypen => Self::Subtract,
                            Token::Ampersand => Self::And,
                            Token::Pipe => Self::Or,
                            Token::Star => return None,
                            _ => unreachable!()
                        }(Box::new(term1), Box::new(term2)))
                    )
                }
                else {
                    None
                }
            }
            None => Some(term1)
        })
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum StatementBody {
    Declaration(Name, DataType),
    SetTo(Accessor, Expression),
    Inc(Accessor),
    Dec(Accessor),
    Write(Expression),
    Read(Accessor),
    While(Expression, Vec<Statement>),
    If(Expression, Vec<Statement>)
}

impl StatementBody {

    fn try_parse(line_num: usize, tokens: &mut Peekable<Iter<Token>>) -> Result<Self, BrainFricError> {
        
        let token1 = match tokens.peek() {
            Some(&token) => token.clone(),
            None => unreachable!()
        };
        
        if token1.is_type_head() {
    
            if let Some(data_type) = DataType::try_parse(tokens) && let Some(Token::Identifier(name)) = tokens.next() {
                return Ok(Self::Declaration(name.clone(), data_type))
            }
            else {
                err!(line_num, ParseError::InvalidExpression)
            }
        }

        tokens.next();

        let token2 = match tokens.next() {
            Some(token) => token,
            None => err!(line_num, ParseError::InvalidExpression)
        };

        Ok(match token1 {

            Token::Inc => {
                if let Some(accessor) = Accessor::try_parse(tokens) {
                    Self::Inc(accessor)
                }
                else {
                    err!(line_num, ParseError::InvalidAccessor);
                }
            }
            Token::Dec => {
                if let Some(accessor) = Accessor::try_parse(tokens) {
                    Self::Dec(accessor)
                }
                else {
                    err!(line_num, ParseError::InvalidAccessor);
                }
            }
            Token::Write => {

                if let Some(expression) = Expression::try_parse(tokens) && tokens.is_empty() {
                    Self::Write(expression)
                }
                else {
                    err!(line_num, ParseError::InvalidExpression);
                }
            }
            Token::WriteLine => {
                Self::Write(Expression::NumberLiteral(10))
            }
            Token::Read => {
                if let Some(accessor) = Accessor::try_parse(tokens) {
                    Self::Read(accessor)
                }
                else {
                    err!(line_num, ParseError::InvalidAccessor);
                }
            }
            _ => {

                if let Some(accessor) = Accessor::try_parse(tokens) && *token2 == Token::SetTo {
                    if let Some(expression) = Expression::try_parse(tokens) && tokens.is_empty() {
                        Self::SetTo(accessor, expression)
                    }
                    else {
                        err!(line_num, ParseError::InvalidExpression);
                    }
                }
                else {
                    err!(line_num, ParseError::InvalidStatement)
                }             
            }
        })
    }

    fn try_parse_control_flow(lines: &mut Vec<Vec<Token>>, condition_tokens: &mut Peekable<Iter<Token>>, line_num: usize) -> Result<(Expression, Vec<Statement>), BrainFricError> {

        condition_tokens.next();

        if let Some(expression) = Expression::try_parse(condition_tokens) && condition_tokens.is_empty() {

            let mut loop_lines = Vec::new();
            let mut loop_depth = 1;

            while loop_depth > 0 {

                if let Some(line) = lines.pop() {

                    match line[0] {
                        Token::If | Token::While => loop_depth += 1,
                        Token::End => loop_depth -= 1,
                        _ => {}
                    }

                    loop_lines.push(line);

                }
                else {
                    err!(line_num, ParseError::ExpectedEnd)
                }
            }

            loop_lines.pop();
            Ok((expression, parse(loop_lines, line_num)?))
        
        }
        else {
            err!(line_num, ParseError::InvalidExpression)
        }
    }
}

#[derive(PartialEq, Eq)]
pub struct Statement {
    pub line_num: usize,
    pub body: StatementBody
}

impl std::fmt::Debug for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {:#?}", self.line_num, self.body)
    }
}

pub fn parse(mut lines: Vec<Vec<Token>>, mut line_num: usize) -> Result<Vec<Statement>, BrainFricError> {

    lines.reverse();

    let mut statements = Vec::new();

    while let Some(line) = lines.pop() {

        line_num += 1;

        let mut line_tokens = line.iter().peekable();

        let body = match line_tokens.peek() {

            Some(Token::If) => {
                let (condition, if_statements) =
                StatementBody::try_parse_control_flow(&mut lines, &mut line_tokens, line_num)?;
                StatementBody::If(condition, if_statements)
            }
            Some(Token::While) => {
                let (condition, while_statements) =
                StatementBody::try_parse_control_flow(&mut lines, &mut line_tokens, line_num)?;
                StatementBody::While(condition, while_statements)
            }
            Some(_) => StatementBody::try_parse(line_num, &mut line_tokens)?,
            None => continue
        };

        statements.push(Statement {
            line_num,
            body
        });

        if !line_tokens.is_empty() {
            err!(line_num, ParseError::InvalidStatement)
        }

    }

    Ok(statements)

}