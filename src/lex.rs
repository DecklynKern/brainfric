use crate::error::*;
use crate::err;

use std::rc::Rc;

pub type Name = Rc<str>;

#[derive(Debug, PartialEq, Eq)]
pub enum Token {

    Identifier(Name),

    BoolLiteral(bool),
    NumberLiteral(i32),
    CharLiteral(char),
    StringLiteral(Rc<str>),

    Bool,
    Byte,
    Short,
    Stack,
    Array,

    Inc,
    Dec,

    While,
    If,
    End,

    Write,
    WriteLine,
    Read,

    OpenParen,
    CloseParen,
    OpenSquare,
    CloseSquare,
    OpenAngle,
    CloseAngle,

    AsBool,
    AsNum,
    Not,

    SetTo,

    Equal,
    Plus,
    Hypen,
    Star,

    At,
    Ampersand,
    Pipe

}

impl Token {

    fn try_parse_keyword(token: &str) -> Option<Self> {
        Some(match token {
            "bool" => Self::Bool,
            "byte" => Self::Byte,
            "short" => Self::Short,
            "stack" => Self::Stack,
            "array" => Self::Array,
            "inc" => Self::Inc,
            "dec" => Self::Dec,
            "while" => Self::While,
            "if" => Self::If,
            "end" => Self::End,
            "write" => Self::Write,
            "writeln" => Self::WriteLine,
            "read" => Self::Read,
            _ => return None
        })
    }

    fn try_parse_bool_literal(token: &str) -> Option<Self> {
        token.parse::<bool>().map(Self::BoolLiteral).ok()
    }
    
    fn try_parse_number_literal(token: &str) -> Option<Self> {
        token.parse::<i32>().map(Self::NumberLiteral).ok()
    }

    fn try_parse_symbols(token: &str) -> Option<Self> {

        Some(match token {
            "(" => Self::OpenParen,
            ")" => Self::CloseParen,
            "[" => Self::OpenSquare,
            "]" => Self::CloseSquare,
            "<" => Self::OpenAngle,
            ">" => Self::CloseAngle,
            "?" => Self::AsBool,
            "#" => Self::AsNum,
            "!" => Self::Not,
            "<-" => Self::SetTo,
            "=" => Self::Equal,
            "+" => Self::Plus,
            "-" => Self::Hypen,
            "*" => Self::Star,
            "@" => Self::At,
            "&" => Self::Ampersand,
            "|" => Self::Pipe,
            _ => return None
        })
    }

    pub fn is_type_head(&self) -> bool {
        matches!(self, Self::Bool | Self::Byte | Self::Short | Self::Stack | Self::Array)
    }

    pub fn is_unary_operator(&self) -> bool {
        matches!(self, Self::AsBool | Self::AsNum | Self::Not)
    }

    pub fn is_binary_operator(&self) -> bool {
        matches!(self, Self::Equal | Self::OpenAngle | Self::CloseAngle | Self::Plus | Self::Hypen | Self::Ampersand | Self::Pipe)
    }
}

pub fn lex(code: &str) -> Result<Vec<Vec<Token>>, BrainFricError> {
    code.split('\n').enumerate().map(lex_line).collect()
}

#[derive(PartialEq, Eq, Debug)]
enum TokenInitialChar {
    Alphabetic,
    Numeric,
    SingleQuote,
    DoubleQuote,
    Other,
    None
}

impl TokenInitialChar {

    fn is_quote(&self) -> bool {
        matches!(self, Self::SingleQuote | Self::DoubleQuote)
    }
}

fn lex_line((line_num, line): (usize, &str)) -> Result<Vec<Token>, BrainFricError> {

    let mut chars: Vec<char> = line.chars().rev().collect();
    chars.insert(0, ' ');
    
    let mut tokens = Vec::new();
    let mut current_token = String::new();
    let mut current_token_initial_char = TokenInitialChar::None;

    while let Some(chr) = chars.pop() {

        let token_over = (chr.is_whitespace() && current_token_initial_char.is_quote()) || chars.is_empty();
        let mut token_ended = true;

        if current_token_initial_char != TokenInitialChar::DoubleQuote && current_token == "//" {
            break; // comment
        }
        else if current_token_initial_char == TokenInitialChar::Alphabetic && 
            (!(chr.is_alphanumeric() || chr == '_') || token_over) {

            if let Some(token) = Token::try_parse_keyword(&current_token) {
                tokens.push(token);
            }
            else if let Some(token) = Token::try_parse_bool_literal(&current_token) {
                tokens.push(token);
            }
            else {
                tokens.push(Token::Identifier(current_token.clone().into()));
            }
        }
        else if current_token_initial_char == TokenInitialChar::Numeric && 
            (!chr.is_numeric() || token_over) {

            if let Some(token) = Token::try_parse_number_literal(&current_token) {
                tokens.push(token);
            }
            else {
                err!(line_num, LexError::InvalidToken(current_token.clone()));
            }
        }
        else if current_token_initial_char == TokenInitialChar::SingleQuote && chr == '\'' {

            if current_token.len() != 2 {
                err!(line_num, LexError::InvalidCharLiteral(current_token.clone()));
            }

            tokens.push(Token::CharLiteral(current_token.chars().nth(1).unwrap()));

            current_token.clear();
            current_token_initial_char = TokenInitialChar::None;
            continue;

        }
        else if current_token_initial_char == TokenInitialChar::DoubleQuote && chr == '"' {
            tokens.push(Token::StringLiteral(current_token[1..].into()));
            current_token.clear();
            current_token_initial_char = TokenInitialChar::None;
            continue;
        }
        else if let Some(token) = Token::try_parse_symbols(&current_token) {

            let mut try_add = current_token.clone();
            try_add.push(chr);

            if Token::try_parse_symbols(&try_add).is_some() {
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
            else if chr == '\'' {
                TokenInitialChar::SingleQuote
            }
            else if chr == '"' {
                TokenInitialChar::DoubleQuote
            }
            else if chr.is_whitespace() {
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

    if !current_token.is_empty() {
        err!(line_num, LexError::InvalidToken(current_token));
    }

    Ok(tokens)

}
