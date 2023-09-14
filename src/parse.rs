use crate::error::*;
use crate::err;
use crate::lex::*;

use std::rc::Rc;
use std::iter::Peekable;
use std::slice::Iter;

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

            //     if tokens.is_empty() {

        //         Some(match token {
        //             Token::Identifier(name) => Self::Identifier(name.clone()),
        //             Token::Literal(Literal::Bool(val)) => Self::BoolLiteral(*val),
        //             Token::Literal(Literal::Number(val)) => Self::NumberLiteral(*val),
        //             Token::Literal(Literal::String(val)) => Self::StringLiteral(val.clone()),
        //             _ => return None
        //         })

        //     } else {

        //         match token {
        //             Token::Separator(Separator::OpenParen) => {

        //                 let mut depth = 1;
                        
        //                 Expression::try_parse(&mut tokens.take_while(|token| match token {
        //                     _ => {
        //                         depth -= 1;
        //                         true
        //                     }
        //                 }).collect::<Iter<Token>>())

        //             }


        //             _ => None

        //         }
        //     }
        // })

        // // else if tokens[0] == Token::Separator(Separator::OpenParen) {
            
        // //     let mut depth = 1;
        // //     let mut idx = 1;

        // //     while depth > 0 && idx < tokens.len() {

        // //         match tokens[idx] {
        // //             Token::Separator(Separator::OpenParen) => depth += 1,
        // //             Token::Separator(Separator::CloseParen) => depth -= 1,
        // //             _ => {}
        // //         }

        // //         idx += 1;

        // //     }

        // //     if depth != 0 {
        // //         return None;
        // //     }

        // //     Expression::try_parse(&tokens[1..idx])

        // // }
        // // else if tokens.len() == 2 {
        // //     if let Token::UnaryOperator(operator) = &tokens[0] {
        // //         Expression::try_parse(&tokens[1..]).map(
        // //             |expr| match operator {
        // //                 UnaryOperator::AsBool => Self::AsBool,
        // //                 UnaryOperator::Not => Self::Not
        // //             }(Box::new(expr))
        // //         )
        // //     } else {
        // //         None
        // //     }
        // // }
        // // else if let Token::BinaryOperator(operator) = &tokens[1] {

        // //     // TODO: actual binary operator parsing that doesn't suck
        // //     Expression::try_parse(&tokens[0..1]).and_then(
        // //         |expr1| Expression::try_parse(&tokens[2..]).map(
        // //             |expr2| match operator {
        // //                 BinaryOperator::Plus => Self::Add,
        // //                 BinaryOperator::Minus => Self::Subtract,
        // //                 BinaryOperator::Equals => Self::Equals,
        // //                 BinaryOperator::GreaterThan => Self::GreaterThan,
        // //                 BinaryOperator::LessThan => Self::LessThan,
        // //                 BinaryOperator::And => Self::And,
        // //                 BinaryOperator::Or => Self::Or,
        // //                 _ => todo!()
        // //             }(Box::new(expr1), Box::new(expr2))
        // //         )
        // //     )
        // // }

    fn try_parse_factor(tokens: &mut Peekable<Iter<Token>>) -> Option<Self> {

        None

    }

    fn try_parse_term(tokens: &mut Peekable<Iter<Token>>) -> Option<Self> {
        Expression::try_parse_factor(tokens)
    }

    fn try_parse(tokens: &mut Peekable<Iter<Token>>) -> Option<Self> {

        tokens.peek().and_then(|token| {

            match token {
                Token::UnaryOperator(op) => {

                }
            }

        })

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

impl StatementBody {

    pub fn uses_variable(&self, variable: &Name) -> bool {

        match self {
            Self::Write(expression) => expression.uses_variable(variable),
            _ => false
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