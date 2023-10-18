use std::rc::Rc;
use std::iter::Peekable;
use std::slice::Iter;

use crate::error::*;
use crate::err;
use crate::lex::*;

macro_rules! expect_token {
    ($tokens: ident, $token: pat) => {
        expect_token!($tokens, $token, {})
    };
    
    ($tokens: ident, $token: pat, $if_token: block) => {
        if let Some($token) = $tokens.next() $if_token
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
    Sequence(Box<DataType>, usize),
    String(usize),
    Stack(Box<DataType>, usize),
    //Array(Rc<DataType>, usize)
}

impl DataType {

    pub fn get_size(&self) -> usize {

        match &self {
            Self::Bool => 1,
            Self::Byte => 1,
            Self::Short => 2,
            Self::Sequence(data_type, len) => len * data_type.get_size(),
            Self::String(len) => len + 2,
            Self::Stack(data_type, len) => len * (data_type.get_size() + 1) + 1,
            //Self::Array(data_type, len) => data_type.get_size() * len
        }
    }

    fn try_parse(tokens: &mut Peekable<Iter<Token>>) -> Option<Self> {

        if tokens.is_empty() || !tokens.peek().unwrap().is_type_head() {
            return None;
        }

        // make a better system for this lol
        Some(match tokens.next().unwrap() {
            Token::Bool => Self::Bool,
            Token::Byte => Self::Byte,
            Token::Short => Self::Short,
            Token::Sequence => return {

                expect_token!(tokens, Token::OpenAngle);

                Self::try_parse(tokens).and_then(|inner_type| {
                    expect_token!(tokens, Token::Comma);
                    match tokens.next() {
                        Some(Token::NumberLiteral(size)) => Some({
                            expect_token!(tokens, Token::CloseAngle);
                            DataType::Sequence(Box::new(inner_type), *size as usize)
                        }),
                        _ => None
                    }
                })
            },
            Token::String => return {

                expect_token!(tokens, Token::OpenAngle);

                match tokens.next() {
                    Some(Token::NumberLiteral(size)) => Some({
                        expect_token!(tokens, Token::CloseAngle);
                        DataType::String(*size as usize)
                    }),
                    _ => None
                }
            },
            Token::Stack => todo!(),
            Token::Array => todo!(),
            _ => unreachable!()
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Specifier {
    ConstIndex(i32)
}

impl Specifier {

    fn try_parse(tokens: &mut Peekable<Iter<Token>>) -> Option<Self> {

        if tokens.peek().is_some_and(|token| token.is_specifier_head()) {

            Some(match tokens.next().unwrap() {
                Token::At => 
                    // change when array happens
                    expect_token!(tokens, Token::NumberLiteral(idx), {Self::ConstIndex(*idx)}),
                _ => unreachable!()
            })
        }
        else {
            None
        }
    }
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

impl Accessor {

    fn try_parse(tokens: &mut Peekable<Iter<Token>>) -> Option<Self> {

        let Some(Token::Identifier(name)) = tokens.peek()
        else {
            return None;
        };

        tokens.next();

        let mut specifiers = Vec::new();

        while let Some(specifier) = Specifier::try_parse(tokens) {
            specifiers.push(specifier);
        }

        Some(Self {
            name: name.clone(),
            specifiers: specifiers.into()
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
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

        if tokens.peek().is_some_and(|token| token.is_factor_head()) {

            match tokens.next().unwrap() {
                Token::OpenParen => Self::try_parse(tokens).and_then(|expr| {
                    expect_token!(tokens, Token::CloseParen);
                    Some(expr)
                }),
                Token::BoolLiteral(val) => Some(Self::BoolLiteral(*val)),
                Token::NumberLiteral(val) => Some(Self::NumberLiteral(*val)),
                Token::CharLiteral(chr) => Some(Self::NumberLiteral(*chr as i32)),
                Token::StringLiteral(val) => Some(Self::StringLiteral(val.clone())),
                _ => unreachable!()
            }
        }
        else {        
            Accessor::try_parse(tokens).map(Self::Access)
        }
    }

    fn try_parse_term(tokens: &mut Peekable<Iter<Token>>) -> Option<Self> {

        let Some(token) = tokens.peek()
        else {
            return None;
        };

        if token.is_unary_operator() {

            let op = tokens.next().unwrap();

            Self::try_parse_term(tokens).map(|term| match op {
                Token::Question => Self::AsBool,
                Token::Pound => Self::AsNum,
                Token::Exclamation => Self::Not,
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

    fn try_parse(tokens: &mut Peekable<Iter<Token>>) -> Option<Self> {

        Expression::try_parse_term(tokens).and_then(|term1| match tokens.peek() {

            Some(&token) => {

                if token.is_binary_operator() {

                    let op = tokens.next().unwrap();
    
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
    Declaration(Vec<Name>, DataType),
    SetTo(Accessor, Expression),
    Inc(Accessor),
    Dec(Accessor),
    Clear(Accessor),
    LeftShift(Accessor, u32),
    RightShift(Accessor, u32),
    Write(Expression),
    Read(Accessor),
    While(Expression, Vec<Statement>),
    If(Expression, Vec<Statement>)
}

impl StatementBody {

    fn try_parse(line_num: usize, tokens: &mut Peekable<Iter<Token>>) -> Result<Self, BrainFricError> {
        
        if let Some(token) = tokens.peek() {
        
            if token.is_type_head() {

                let (Some(data_type), Some(Token::Identifier(name1))) = (DataType::try_parse(tokens), tokens.next())
                else {
                    err!(line_num, ParseError::InvalidStatement)
                };

                let mut names = vec![name1.clone()];

                while !tokens.is_empty() {
                    
                    let (Some(Token::Comma), Some(Token::Identifier(name))) = (tokens.next(), tokens.next())
                    else {
                        err!(line_num, ParseError::InvalidStatement)
                    };

                    names.push(name.clone());

                }
    
                return Ok(Self::Declaration(names, data_type));
            
            }
        }
        else {
            unreachable!()
        }

        if let (Some(accessor), Some(Token::SetTo)) = (Accessor::try_parse(tokens), tokens.peek()) {

            tokens.next();
        
            if let (Some(expression), true) = (Expression::try_parse(tokens), tokens.is_empty()) {
                return Ok(Self::SetTo(accessor, expression));
            }
            else {
                err!(line_num, ParseError::InvalidExpression);
            }
        }

        let token1 = tokens.next().unwrap();

        Ok(match token1 {

            Token::Inc | Token::Dec | Token:: Clear | Token::Read => {
                
                let (Some(accessor), true) = (Accessor::try_parse(tokens), tokens.is_empty())
                else {
                    err!(line_num, ParseError::InvalidAccessor);
                };

                match token1 {
                    Token::Inc => Self::Inc(accessor),
                    Token::Dec => Self::Dec(accessor),
                    Token::Clear => Self::Clear(accessor),
                    Token::Read => Self::Read(accessor),
                    _ => unreachable!()
                }
            }
            Token::LeftShift | Token::RightShift => {

                if !matches!(tokens.next(), Some(Token::OpenAngle)) {
                    err!(line_num, ParseError::ExpectedOpenAngle);
                }

                let Some(Token::NumberLiteral(offset)) = tokens.next()
                else {
                    err!(line_num, ParseError::ExpectedNumberLiteral);
                };

                if !matches!(tokens.next(), Some(Token::CloseAngle)) {
                    err!(line_num, ParseError::ExpectedCloseAngle);
                }
                
                let Some(accessor) = Accessor::try_parse(tokens)
                else {
                    err!(line_num, ParseError::InvalidAccessor);
                };

                if *token1 == Token::LeftShift {
                    Self::LeftShift(accessor, *offset as u32)
                }
                else {
                    Self::RightShift(accessor, *offset as u32)
                }
            }
            Token::Write => {

                let (Some(expression), true) = (Expression::try_parse(tokens), tokens.is_empty())
                else {
                    err!(line_num, ParseError::InvalidExpression);
                };
                
                Self::Write(expression)
                
            }
            Token::WriteLine => {
                Self::Write(Expression::NumberLiteral(10))
            }
            _ => err!(line_num, ParseError::InvalidStatement)
        })
    }

    fn try_parse_control_flow(lines: &mut Vec<Vec<Token>>, condition_tokens: &mut Peekable<Iter<Token>>, line_num: usize) -> Result<(Expression, Vec<Statement>), BrainFricError> {

        condition_tokens.next();

        let (Some(expression), true) = (Expression::try_parse(condition_tokens), condition_tokens.is_empty())
        else {
            err!(line_num, ParseError::InvalidExpression);
        };

        let mut loop_lines = Vec::new();
        let mut loop_depth = 1;

        while loop_depth > 0 {

            let Some(line) = lines.pop()
            else {
                err!(line_num, ParseError::ExpectedEnd);
            };

            match line[0] {
                Token::If | Token::While => loop_depth += 1,
                Token::End => loop_depth -= 1,
                 _ => {}
            }

            loop_lines.push(line);
            
        }

        loop_lines.pop();
        Ok((expression, parse(loop_lines, line_num)?))
        
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
