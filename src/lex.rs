use crate::error::*;
use crate::err;

use std::rc::Rc;

#[derive(Debug, PartialEq, Eq)]
pub enum Type {
    Bool,
    Byte,
    Short,
    Stack,
    Array,
}

impl Type {

    fn try_parse(token: &String) -> Option<Token> {
        Some(Token::Type(match token.as_str() {
            "bool" => Self::Bool,
            "byte" => Self::Byte,
            "short" => Self::Short,
            "stack" => Self::Stack,
            "array" => Self::Array,
            _ => return None
        }))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Keyword {

    Inc,
    Dec,

    While,
    If,
    End,

    Write,
    WriteLine,
    Read

}

impl Keyword {

    fn try_parse(token: &String) -> Option<Token> {
        Some(Token::Keyword(match token.as_str() {
            "inc" => Self::Inc,
            "dec" => Self::Dec,
            "while" => Self::While,
            "if" => Self::If,
            "end" => Self::End,
            "write" => Self::Write,
            "writeln" => Self::WriteLine,
            "read" => Self::Read,
            _ => return None
        }))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Literal {
    Bool(bool),
    Number(usize),
    String(Rc<str>)
}

impl Literal {
    
    // lol
    fn try_parse_bool(token: &String) -> Option<Token> {
        token.parse::<bool>().map(|literal|Token::Literal(Self::Bool(literal))).ok()
    }
    
    fn try_parse_number(token: &String) -> Option<Token> {
        token.parse::<usize>().map(|literal|Token::Literal(Self::Number(literal))).ok()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Separator {
    OpenParen,
    CloseParen,
    OpenSquare,
    CloseSquare
}

impl Separator {
    
    fn try_parse(token: &String) -> Option<Token> {
        Some(Token::Separator(match token.as_str() {
            "(" => Self::OpenParen,
            ")" => Self::CloseParen,
            "[" => Self::OpenSquare,
            "]" => Self::CloseSquare,
            _ => return None
        }))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum UnaryOperator {
    AsBool,
    Not
}

impl UnaryOperator {
    
    fn try_parse(token: &String) -> Option<Token> {

        Some(Token::UnaryOperator(match token.as_str() {
            "?" => Self::AsBool,
            "!" => Self::Not,
            _ => return None
        }))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum BinaryOperator {

    SetTo,

    Equals,
    LessThan,
    GreaterThan,
    Plus,
    Minus,
    Times,

    And,
    Or

}

impl BinaryOperator {
    
    fn try_parse(token: &String) -> Option<Token> {

        Some(Token::BinaryOperator(match token.as_str() {
            "<-" => Self::SetTo,
            "=" => Self::Equals,
            "<" => Self::LessThan,
            ">" => Self::GreaterThan,
            "+" => Self::Plus,
            "-" => Self::Minus,
            "*" => Self::Times,
            "&" => Self::And,
            "|" => Self::Or,
            _ => return None
        }))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Identifier(String),
    Type(Type),
    Keyword(Keyword),
    Literal(Literal),
    Separator(Separator),
    UnaryOperator(UnaryOperator),
    BinaryOperator(BinaryOperator)
}

#[derive(PartialEq, Eq, Debug)]
enum TokenInitialChar {
    Alphabetic,
    Numeric,
    Quote,
    Other,
    None
}

pub fn lex(code: &str) -> Result<Vec<Vec<Token>>, BrainFricError> {
    code.split("\n").enumerate().map(lex_line).into_iter().collect()
}

fn lex_line((line_num, line): (usize, &str)) -> Result<Vec<Token>, BrainFricError> {

    let mut chars: Vec<char> = line.chars().rev().collect();
    chars.insert(0, ' ');
    
    let mut tokens = Vec::new();
    let mut current_token = String::new();
    let mut current_token_initial_char = TokenInitialChar::None;

    while let Some(chr) = chars.pop() {

        let token_over = (chr.is_whitespace() && current_token_initial_char != TokenInitialChar::Quote) || chars.is_empty();
        let mut token_ended = true;

        if current_token_initial_char != TokenInitialChar::Quote && current_token == "//" {
            break; // comment
        }
        else if current_token_initial_char == TokenInitialChar::Alphabetic && 
            (!(chr.is_alphanumeric() || chr == '_') || token_over) {

            if let Some(token) = Keyword::try_parse(&current_token) {
                tokens.push(token);
            }
            else if let Some(token) = Type::try_parse(&current_token) {
                tokens.push(token);
            }
            else if let Some(token) = Literal::try_parse_bool(&current_token) {
                tokens.push(token);
            }
            else {
                tokens.push(Token::Identifier(current_token.clone()));
            }
        }
        else if current_token_initial_char == TokenInitialChar::Numeric && 
            (!chr.is_numeric() || token_over) {

            if let Some(token) = Literal::try_parse_number(&current_token) {
                tokens.push(token);
            }
            else {
                err!(line_num, LexError::InvalidToken(current_token.clone()));
            }
        }
        else if current_token_initial_char == TokenInitialChar::Quote && chr == '"' {
            tokens.push(Token::Literal(Literal::String(current_token[1..].into())));
            current_token.clear();
            current_token_initial_char = TokenInitialChar::None;
            continue;
        }
        else if let Some(token) = Separator::try_parse(&current_token) {
            tokens.push(token);
        }
        else if let Some(token) = UnaryOperator::try_parse(&current_token) {
            tokens.push(token);
        }
        else if let Some(token) = BinaryOperator::try_parse(&current_token) {

            let mut try_add = current_token.clone();
            try_add.push(chr);

            if let Some(_) = BinaryOperator::try_parse(&try_add) {
                current_token.push(chr);
                continue;
            }
            else {
                tokens.push(token);
            }
        }
        else if !token_over {
            token_ended = false;
        }

        if current_token_initial_char == TokenInitialChar::None || token_ended {

            current_token.clear();
        
            current_token_initial_char = if chr.is_alphabetic() {
                TokenInitialChar::Alphabetic
            }
            else if chr.is_numeric() {
                TokenInitialChar::Numeric
            }
            else if chr == '"' {
                TokenInitialChar::Quote
            }
            else if chr == ' ' {
                TokenInitialChar::None
            }
            else {
                TokenInitialChar::Other
            };
        }

        if !token_over {
            current_token.push(chr);
        }
    }

    Ok(tokens)

}
