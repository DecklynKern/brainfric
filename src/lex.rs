use crate::error::*;
use crate::err;

use std::rc::Rc;
use std::str::Chars;
use std::iter::Peekable;

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
    Sequence,
    Stack,
    Array,

    Inc,
    Dec,

    Clear,
    LeftShift,
    RightShift,

    While,
    If,
    End,

    Write,
    WriteLine,
    Read,

    Comma,

    OpenParen,
    CloseParen,
    OpenSquare,
    CloseSquare,
    OpenAngle,
    CloseAngle,

    Question,
    Pound,
    Exclamation,

    SetTo,

    Equal,
    Plus,
    Hypen,
    Star,

    At,
    Ampersand,
    Pipe,
    Dollar

}

impl Token {

    fn try_parse_keyword(token: &str) -> Option<Self> {
        Some(match token {
            "bool" => Self::Bool,
            "byte" => Self::Byte,
            "short" => Self::Short,
            "seq" => Self::Sequence,
            "stack" => Self::Stack,
            "array" => Self::Array,
            "inc" => Self::Inc,
            "dec" => Self::Dec,
            "clear" => Self::Clear,
            "lshift" => Self::LeftShift,
            "rshift" => Self::RightShift,
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
            "," => Self::Comma,
            "(" => Self::OpenParen,
            ")" => Self::CloseParen,
            "[" => Self::OpenSquare,
            "]" => Self::CloseSquare,
            "<" => Self::OpenAngle,
            ">" => Self::CloseAngle,
            "?" => Self::Question,
            "#" => Self::Pound,
            "!" => Self::Exclamation,
            "<-" => Self::SetTo,
            "=" => Self::Equal,
            "+" => Self::Plus,
            "-" => Self::Hypen,
            "*" => Self::Star,
            "@" => Self::At,
            "&" => Self::Ampersand,
            "|" => Self::Pipe,
            "$" => Self::Dollar,
            _ => return None
        })
    }

    pub fn is_type_head(&self) -> bool {
        matches!(self,
            Self::Bool |
            Self::Byte |
            Self::Short |
            Self::Sequence |
            Self::Stack |
            Self::Array
        )
    }

    pub fn is_specifier_head(&self) -> bool {
        matches!(self,
            Self::At
        )
    }

    pub fn is_factor_head(&self) -> bool {
        matches!(self,
            Self::OpenParen |
            Self::BoolLiteral(_) |
            Self::NumberLiteral(_) |
            Self::CharLiteral(_) |
            Self::StringLiteral(_)
        )
    }

    pub fn is_unary_operator(&self) -> bool {
        matches!(self,
            Self::Question |
            Self::Pound |
            Self::Exclamation
        )
    }

    pub fn is_binary_operator(&self) -> bool {
        matches!(self,
            Self::Equal |
            Self::OpenAngle |
            Self::CloseAngle |
            Self::Plus |
            Self::Hypen |
            Self::Ampersand |
            Self::Pipe
        )
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

    let mut tokens = Vec::new();
    let mut chars = line.chars().peekable();

    while let Some(char) = chars.peek() {

        match lex_char_literal(&mut chars) {
            Ok(Some(char_literal)) => tokens.push(Token::CharLiteral(char_literal)),
            Ok(None) => {}
            Err(()) => err!(line_num, LexError::InvalidCharLiteral)
        }

        match lex_string_literal(&mut chars) {
            Ok(Some(string_literal)) => tokens.push(Token::StringLiteral(string_literal)),
            Ok(None) => {}
            Err(()) => err!(line_num, LexError::InvalidStringLiteral)
        }
        if let Some(num) = lex_number_literal(&mut chars) {
            tokens.push(Token::NumberLiteral(num));
        }
        
        err!(line_num, LexError::InvalidToken);

    }

    todo!("rewrite lexer");

    Ok(tokens)

    // let mut chars: Vec<char> = line.chars().rev().collect();
    // chars.insert(0, ' ');
    
    // let mut current_token = String::new();
    // let mut current_token_initial_char = TokenInitialChar::None;

    // while let Some(chr) = chars.pop() {

    //     let token_over = (chr.is_whitespace() && current_token_initial_char.is_quote()) || chars.is_empty();
    //     let mut token_ended = true;

    //     if current_token_initial_char != TokenInitialChar::DoubleQuote && current_token == "//" {
    //         return Ok(tokens);
    //     }
    //     else if current_token_initial_char == TokenInitialChar::Alphabetic && 
    //         (!(chr.is_alphanumeric() || chr == '_') || token_over) {

    //         if let Some(token) = Token::try_parse_keyword(&current_token) {
    //             tokens.push(token);
    //         }
    //         else if let Some(token) = Token::try_parse_bool_literal(&current_token) {
    //             tokens.push(token);
    //         }
    //         else {
    //             tokens.push(Token::Identifier(current_token.clone().into()));
    //         }
    //     }
    //     else if current_token_initial_char == TokenInitialChar::Numeric && 
    //         (!chr.is_numeric() || token_over) {

    //         if let Some(token) = Token::try_parse_number_literal(&current_token) {
    //             tokens.push(token);
    //         }
    //         else {
    //             err!(line_num, LexError::InvalidToken(current_token.clone()));
    //         }
    //     }
    //     else if current_token_initial_char == TokenInitialChar::SingleQuote && chr == '\'' {

    //         if current_token.len() != 2 {
    //             err!(line_num, LexError::InvalidCharLiteral(current_token.clone()));
    //         }

    //         tokens.push(Token::CharLiteral(current_token.chars().nth(1).unwrap()));

    //         current_token.clear();
    //         current_token_initial_char = TokenInitialChar::None;
    //         continue;

    //     }
    //     else if current_token_initial_char == TokenInitialChar::DoubleQuote && chr == '"' {
    //         tokens.push(Token::StringLiteral(current_token[1..].into()));
    //         current_token.clear();
    //         current_token_initial_char = TokenInitialChar::None;
    //         continue;
    //     }
    //     else if let Some(token) = Token::try_parse_symbols(&current_token) {

    //         let mut try_add = current_token.clone();
    //         try_add.push(chr);

    //         if Token::try_parse_symbols(&try_add).is_some() {
    //             current_token.push(chr);
    //             continue;
    //         }
    //         else {
    //             tokens.push(token);
    //         }
    //     }
    //     else if !token_over {
    //         token_ended = false;
    //     }

    //     if current_token_initial_char == TokenInitialChar::None || token_ended {

    //         current_token.clear();
        
    //         current_token_initial_char = if chr.is_alphabetic() {
    //             TokenInitialChar::Alphabetic
    //         }
    //         else if chr.is_numeric() {
    //             TokenInitialChar::Numeric
    //         }
    //         else if chr == '\'' {
    //             TokenInitialChar::SingleQuote
    //         }
    //         else if chr == '"' {
    //             TokenInitialChar::DoubleQuote
    //         }
    //         else if chr.is_whitespace() {
    //             TokenInitialChar::None
    //         }
    //         else {
    //             TokenInitialChar::Other
    //         };
    //     }

    //     if !token_over {
    //         current_token.push(chr);
    //     }
    // }

    // if !current_token.is_empty() {
    // }

    // Ok(tokens)

}

fn lex_char_literal(chars: &mut Peekable<Chars>) -> Result<Option<char>, ()> {

    if chars.peek() != Some(&'\'') {
        return Ok(None);
    }

    chars.next();

    let Some(char) = chars.next()
    else {
        return Err(());
    };

    if chars.next() == Some('\'') {
        Ok(Some(char))

    } else {
        Err(())
    }
}

fn lex_string_literal(chars: &mut Peekable<Chars>) -> Result<Option<Rc<str>>, ()> {

    if chars.peek() != Some(&'"') {
        return Ok(None);
    }

    chars.next();

    let mut string_literal = String::new();

    while let Some(char) = chars.next() {

        if char == '"' {
            return Ok(Some(string_literal.into()));
        }
        
        string_literal.push(char);

    }

    Err(())

}

fn lex_number_literal(chars: &mut Peekable<Chars>) -> Option<i32> {
    
    let Some(char) = chars.peek()
    else {
        return None;
    };

    let mut number_chars = String::new();

    while let Some(char) = chars.peek() && char.is_numeric() {
        number_chars.push(*char);
        chars.next();
    }

    if number_chars.is_empty() {
        None
    }
    else {
        Some(number_chars.parse().unwrap())
    }
}