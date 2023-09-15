use std::rc::Rc;
use std::iter::Peekable;
use std::slice::Iter;

use crate::error::*;
use crate::err;
use crate::lex::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DataType {
    Bool,
    Byte,
    Short,
    Stack(usize),
    Array(Box<DataType>, usize)
}

impl DataType {

    pub fn get_size(&self) -> usize {

        match &self {
            Self::Bool => 1,
            Self::Byte => 1,
            Self::Short => 2,
            Self::Stack(len) => *len,
            Self::Array(data_type, len) => data_type.get_size() * len
        }
    }
}

pub type Name = String;

#[derive(Debug, PartialEq, Eq)]
pub enum Expression {
    Identifier(Name),
    BoolLiteral(bool),
    NumberLiteral(usize),
    StringLiteral(Rc<str>),
    Equals(Box<Expression>, Box<Expression>),
    LessThan(Box<Expression>, Box<Expression>),
    GreaterThan(Box<Expression>, Box<Expression>),
    Not(Box<Expression>),
    And(Box<Expression>, Box<Expression>),
    Or(Box<Expression>, Box<Expression>),
    Add(Box<Expression>, Box<Expression>),
    Subtract(Box<Expression>, Box<Expression>),
    AsBool(Box<Expression>)
}

impl Expression {

    fn try_parse_factor(tokens: &mut Peekable<Iter<Token>>) -> Option<Self> {

        tokens.next().and_then(|token| {

            match token {
                Token::Identifier(name) => Some(Self::Identifier(name.clone())),
                Token::Literal(Literal::Bool(val)) => Some(Self::BoolLiteral(*val)),
                Token::Literal(Literal::Number(val)) => Some(Self::NumberLiteral(*val)),
                Token::Literal(Literal::String(val)) => Some(Self::StringLiteral(val.clone())),
                Token::Separator(Separator::OpenParen) => {

                    Expression::try_parse(tokens).and_then(|expr| {

                        tokens.next().and_then(|next_token| match next_token {
                            Token::Separator(Separator::CloseParen) => Some(expr),
                            _ => None
                        })  
                    })
                }
                _ => None
            }
        })
    }

    fn try_parse_term(tokens: &mut Peekable<Iter<Token>>) -> Option<Self> {

        match tokens.peek() {
            Some(token) => {
                match token {
                    Token::UnaryOperator(op) => {
    
                        tokens.next();
    
                        Expression::try_parse_term(tokens).map(|term| match op {
                            UnaryOperator::AsBool => Self::AsBool,
                            UnaryOperator::Not => Self::Not
                        }(Box::new(term)))
                    }
                    _ => Expression::try_parse_factor(tokens)
                }
            }
            None => None
        }
    }

    fn try_parse(tokens: &mut Peekable<Iter<Token>>) -> Option<Self> {

        Expression::try_parse_term(tokens).and_then(|term1| {

            match tokens.peek() {
                Some(Token::BinaryOperator(op)) => {

                    tokens.next();

                    Expression::try_parse_term(tokens).and_then(|term2| {
                        
                        Some(match op {
                            BinaryOperator::Equals => Self::Equals,
                            BinaryOperator::LessThan => Self::LessThan,
                            BinaryOperator::GreaterThan => Self::GreaterThan,
                            BinaryOperator::Plus => Self::Add,
                            BinaryOperator::Minus => Self::Subtract,
                            BinaryOperator::And => Self::And,
                            BinaryOperator::Or => Self::Or,
                            _ => {return None;}
                        }(Box::new(term1), Box::new(term2)))
                    })
                }
                Some(_) => None,
                None => Some(term1)
            }
        })
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum StatementBody {
    Declaration(Name, DataType),
    SetTo(Name, Expression),
    Inc(Name),
    Dec(Name),
    Write(Expression),
    Read(Name),
    While(Expression, Vec<Statement>),
    If(Expression, Vec<Statement>)
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

fn parse_control_flow_statment(tokens: &mut Vec<Vec<Token>>, current_line: &Vec<Token>, current_line_num: usize) -> Result<(Expression, Vec<Statement>), BrainFricError> {

    Expression::try_parse(&mut current_line[1..].iter().peekable()).map(|expression| {

        let mut loop_lines = Vec::new();
        let mut loop_depth = 1;

        while loop_depth > 0 {

            if let Some(line) = tokens.pop() {

                match line[0] {
                    Token::Keyword(Keyword::If | Keyword::While) => loop_depth += 1,
                    Token::Keyword(Keyword::End) => loop_depth -= 1,
                    _ => {}
                }

                loop_lines.push(line);

            }
            else {
                err!(current_line_num, ParseError::ExpectedEnd)
            }
        }

        loop_lines.pop();
        Ok((expression, parse(loop_lines, current_line_num)?))

    }).unwrap_or_else(|| err!(current_line_num, ParseError::InvalidExpression))

}

pub fn parse(mut tokens: Vec<Vec<Token>>, mut current_line_num: usize) -> Result<Vec<Statement>, BrainFricError> {

    tokens.reverse();

    let mut statements = Vec::new();

    while let Some(line) = tokens.pop() {

        current_line_num += 1;

        if line.len() < 1 {
            continue;
        }

        let body = match &line[0] {
            Token::Type(var_type) if let Token::Identifier(name) = &line[1] => {

                if *var_type == Type::Bool {
                    StatementBody::Declaration(name.clone(), DataType::Bool) 
                }
                else if *var_type == Type::Byte {
                    StatementBody::Declaration(name.clone(), DataType::Byte)
                }
                else if *var_type == Type::Short {
                    StatementBody::Declaration(name.clone(), DataType::Short)
                }
                else if *var_type == Type::Array {
                    todo!();
                }
                else {
                    err!(current_line_num, ParseError::InvalidStatement);
                }
            }
            Token::Identifier(name) if Token::BinaryOperator(BinaryOperator::SetTo) == line[1] => {
                if let Some(expression) = Expression::try_parse(&mut line[2..].iter().peekable()) {
                    StatementBody::SetTo(name.clone(), expression)
                }
                else {
                    err!(current_line_num, ParseError::InvalidExpression);
                }
            }
            Token::Keyword(Keyword::Inc) => {
                if let Token::Identifier(name) = &line[1] {
                    StatementBody::Inc(name.clone())
                }
                else {
                    err!(current_line_num, ParseError::ExpectedIdentifier);
                }
            }
            Token::Keyword(Keyword::Dec) => {
                if let Token::Identifier(name) = &line[1] {
                    StatementBody::Dec(name.clone())
                }
                else {
                    err!(current_line_num, ParseError::ExpectedIdentifier);
                }
            }
            Token::Keyword(Keyword::Write) => {
                if let Some(expression) = Expression::try_parse(&mut line[1..].iter().peekable()) {
                    StatementBody::Write(expression)
                }
                else {
                    err!(current_line_num, ParseError::InvalidExpression);
                }
            }
            Token::Keyword(Keyword::WriteLine) => {
                StatementBody::Write(Expression::NumberLiteral(10))
            }
            Token::Keyword(Keyword::Read) => {
                if let Token::Identifier(name) = &line[1] {
                    StatementBody::Read(name.clone())
                }
                else {
                    err!(current_line_num, ParseError::ExpectedIdentifier);
                }
            }
            Token::Keyword(Keyword::While) => {
                let (expression, loop_statements) = 
                    parse_control_flow_statment(&mut tokens, &line, current_line_num)?;
                StatementBody::While(expression, loop_statements)
            }
            Token::Keyword(Keyword::If) => {
                let (expression, loop_statements) = 
                    parse_control_flow_statment(&mut tokens, &line, current_line_num)?;
                StatementBody::If(expression, loop_statements)
            }
            _ => err!(current_line_num, ParseError::InvalidStatement)
        };

        statements.push(Statement {
            line_num: current_line_num,
            body
        });
    }

    Ok(statements)

}