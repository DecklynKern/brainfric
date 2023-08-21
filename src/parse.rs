use crate::error::*;
use crate::err;
use crate::lex::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DataType {
    Bool,
    Byte,
    Short,
    Array(Box<DataType>, usize)
}

impl DataType {

    pub fn get_size(&self) -> usize {

        match &self {
            Self::Bool => 1,
            Self::Byte => 1,
            Self::Short => 2,
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
    StringLiteral(String),
    Equals(Box<Expression>, Box<Expression>),
    LessThan(Box<Expression>, Box<Expression>),
    GreaterThan(Box<Expression>, Box<Expression>),
    Add(Box<Expression>, Box<Expression>),
    Subtract(Box<Expression>, Box<Expression>)
}

impl Expression {

    fn try_parse(tokens: &[Token]) -> Option<Expression> {

        if tokens.is_empty() {
            return None;
        }

        if tokens.len() == 1 {
            if let Token::Identifier(name) = &tokens[0] {
                return Some(Self::Identifier(name.clone()));
            
            } else if let Token::Literal(Literal::Bool(val)) = tokens[0] {
                return Some(Self::BoolLiteral(val));

            } else if let Token::Literal(Literal::Number(val)) = tokens[0] {
                return Some(Self::NumberLiteral(val));

            } else if let Token::Literal(Literal::String(val)) = &tokens[0] {
                return Some(Self::StringLiteral(val.clone()));
            }
        }

        None

    }

    fn uses_variable(&self, variable: &Name) -> bool {
        
        match self {
            Self::Identifier(name) => *name == *variable,
            Self::Equals(expr1, expr2) | Self::LessThan(expr1, expr2) | Self::GreaterThan(expr1, expr2) | Self::Add(expr1, expr2) | Self::Subtract(expr1, expr2) =>
                expr1.uses_variable(variable) || expr2.uses_variable(variable),
            _ => false
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Statement {
    Declaration(Name, DataType),
    MoveTo(Name, Expression),
    SetTo(Name, Expression),
    Inc(Name),
    Dec(Name),
    Write(Expression),
    Read(Name)
}

impl Statement {

    pub fn uses_variable(&self, variable: &Name) -> bool {

        match self {
            Self::Write(expression) => expression.uses_variable(variable),
            _ => false
        }
    }
}

pub struct Parser {
    tokens: Vec<Vec<Token>>,
    current_line_num: usize
}

impl Parser {

    pub fn new(mut tokens: Vec<Vec<Token>>) -> Self {

        tokens.reverse();

        Self {
            tokens,
            current_line_num: 0
        }
    }

    pub fn parse(&mut self) -> Result<Vec<(usize, Statement)>, BrainFricError> {

        let mut statements = Vec::new();

        while let Some(line) = self.tokens.pop() {
    
            if line.is_empty() {
                self.current_line_num += 1;
                continue;
            }
    
            if line.len() == 1 {
                self.current_line_num += 1;
                continue;
            }
    
            statements.push((self.current_line_num, match &line[0] {
                Token::Keyword(keyword) if let Token::Identifier(name) = &line[1] && keyword.is_type() => {
    
                    if *keyword == Keyword::Bool {
                        Statement::Declaration(name.clone(), DataType::Bool)
                        
                    } else if *keyword == Keyword::Byte {
                        Statement::Declaration(name.clone(), DataType::Byte)
                    
                    } else if *keyword == Keyword::Short {
                        Statement::Declaration(name.clone(), DataType::Short)
    
                    } else if *keyword == Keyword::Array {
                        todo!();
    
                    } else {
                        err!(self.current_line_num, ParserError::InvalidStatement);
                    }
                }
                Token::Identifier(name) if Token::Operator(Operator::MoveTo) == line[1] => {
                    if let Some(expression) = Expression::try_parse(&line[2..]) {
                        Statement::SetTo(name.clone(), expression)
                    
                    } else {
                        err!(self.current_line_num, ParserError::InvalidExpression);
                    }
                }
                Token::Identifier(name) if Token::Operator(Operator::SetTo) == line[1] => {
                    if let Some(expression) = Expression::try_parse(&line[2..]) {
                        Statement::SetTo(name.clone(), expression)
                    
                    } else {
                        err!(self.current_line_num, ParserError::InvalidExpression);
                    }
                }
                Token::Keyword(Keyword::Inc) => {
                    if let Token::Identifier(name) = &line[1] {
                        Statement::Inc(name.clone())
                    
                    } else {
                        err!(self.current_line_num, ParserError::ExpectedIdentifier);
                    }
                }
                Token::Keyword(Keyword::Dec) => {
                    if let Token::Identifier(name) = &line[1] {
                        Statement::Dec(name.clone())
                    
                    } else {
                        err!(self.current_line_num, ParserError::ExpectedIdentifier);
                    }
                }
                Token::Keyword(Keyword::Write) => {
                    if let Some(expression) = Expression::try_parse(&line[1..]) {
                        Statement::Write(expression)
                    
                    } else {
                        err!(self.current_line_num, ParserError::InvalidExpression);
                    }
                }
                Token::Keyword(Keyword::Read) => {
                    if let Token::Identifier(name) = &line[1] {
                        Statement::Read(name.clone())
                    
                    } else {
                        err!(self.current_line_num, ParserError::ExpectedIdentifier);
                    }
                }
                _ => err!(self.current_line_num, ParserError::InvalidStatement)
            }));

            self.current_line_num += 1;

        }

        Ok(statements)

    }
}