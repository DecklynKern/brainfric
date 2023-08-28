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
    Not(Box<Expression>),
    And(Box<Expression>, Box<Expression>),
    Or(Box<Expression>, Box<Expression>),
    Add(Box<Expression>, Box<Expression>),
    Subtract(Box<Expression>, Box<Expression>),
    AsBool(Box<Expression>)
}

impl Expression {

    fn try_parse(tokens: &[Token]) -> Option<Self> {

        if tokens.is_empty() {
            None
        }
        else if tokens.len() == 1 {
            if let Token::Identifier(name) = &tokens[0] {
                Some(Self::Identifier(name.clone()))
            }
            else if let Token::Literal(Literal::Bool(val)) = tokens[0] {
                Some(Self::BoolLiteral(val))
            }
            else if let Token::Literal(Literal::Number(val)) = tokens[0] {
                Some(Self::NumberLiteral(val))
            }
            else if let Token::Literal(Literal::String(val)) = &tokens[0] {
                Some(Self::StringLiteral(val.clone()))

            } else {
                None
            }
        }
        else if tokens[0] == Token::Operator(Operator::AsBool) {
            if let Some(expr) = Expression::try_parse(&tokens[1..]) {
                Some(Self::AsBool(Box::new(expr)))
            }
            else {
                None
            }
        } else if tokens[1] == Token::Operator(Operator::Plus) {
            if let Some(expr1) = Expression::try_parse(&tokens[0..1]) {
                if let Some(expr2) = Expression::try_parse(&tokens[2..]) {
                    Some(Self::Add(Box::new(expr1), Box::new(expr2)))
                }
                else {
                    None
                }
            }
            else {
                None
            }
        }
        else {
            None
        }
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
    SetTo(Name, Expression),
    Inc(Name),
    Dec(Name),
    Write(Expression),
    Read(Name),
    While(Expression, Vec<(usize, Statement)>),
    If(Expression, Vec<(usize, Statement)>)
}

impl Statement {

    pub fn uses_variable(&self, variable: &Name) -> bool {

        match self {
            Self::Write(expression) => expression.uses_variable(variable),
            _ => false
        }
    }
}

fn parse_control_flow_statment(tokens: &mut Vec<Vec<Token>>, current_line: &Vec<Token>, current_line_num: usize) -> Result<(Expression, Vec<(usize, Statement)>), BrainFricError> {

    if let Some(expression) = Expression::try_parse(&current_line[1..]) {

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

    }
    else {
        err!(current_line_num, ParseError::InvalidExpression);
    }
}

pub fn parse(mut tokens: Vec<Vec<Token>>, mut current_line_num: usize) -> Result<Vec<(usize, Statement)>, BrainFricError> {

    tokens.reverse();

    let mut statements = Vec::new();

    while let Some(line) = tokens.pop() {

        current_line_num += 1;

        if line.len() < 2 {
            continue;
        }

        statements.push((current_line_num, match &line[0] {
            Token::Keyword(keyword) if let Token::Identifier(name) = &line[1] && keyword.is_type() => {

                if *keyword == Keyword::Bool {
                    Statement::Declaration(name.clone(), DataType::Bool) 
                }
                else if *keyword == Keyword::Byte {
                    Statement::Declaration(name.clone(), DataType::Byte)
                }
                else if *keyword == Keyword::Short {
                    Statement::Declaration(name.clone(), DataType::Short)
                }
                else if *keyword == Keyword::Array {
                    todo!();
                }
                else {
                    err!(current_line_num, ParseError::InvalidStatement);
                }
            }
            Token::Identifier(name) if Token::Operator(Operator::SetTo) == line[1] => {
                if let Some(expression) = Expression::try_parse(&line[2..]) {
                    Statement::SetTo(name.clone(), expression)
                }
                else {
                    err!(current_line_num, ParseError::InvalidExpression);
                }
            }
            Token::Keyword(Keyword::Inc) => {
                if let Token::Identifier(name) = &line[1] {
                    Statement::Inc(name.clone())
                }
                else {
                    err!(current_line_num, ParseError::ExpectedIdentifier);
                }
            }
            Token::Keyword(Keyword::Dec) => {
                if let Token::Identifier(name) = &line[1] {
                    Statement::Dec(name.clone())
                }
                else {
                    err!(current_line_num, ParseError::ExpectedIdentifier);
                }
            }
            Token::Keyword(Keyword::Write) => {
                if let Some(expression) = Expression::try_parse(&line[1..]) {
                    Statement::Write(expression)
                }
                else {
                    err!(current_line_num, ParseError::InvalidExpression);
                }
            }
            Token::Keyword(Keyword::Read) => {
                if let Token::Identifier(name) = &line[1] {
                    Statement::Read(name.clone())
                }
                else {
                    err!(current_line_num, ParseError::ExpectedIdentifier);
                }
            }
            Token::Keyword(Keyword::While) => {
                let (expression, loop_statements) = 
                    parse_control_flow_statment(&mut tokens, &line, current_line_num)?;
                Statement::While(expression, loop_statements)
            }
            Token::Keyword(Keyword::If) => {
                let (expression, loop_statements) = 
                    parse_control_flow_statment(&mut tokens, &line, current_line_num)?;
                Statement::If(expression, loop_statements)
            }
            _ => err!(current_line_num, ParseError::InvalidStatement)
        }));
    }

    Ok(statements)

}