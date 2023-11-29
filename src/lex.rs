use crate::error::*;
use crate::err;

use std::rc::Rc;
use std::str::Chars;
use std::iter::Peekable;

pub type Name = Rc<str>;

#[derive(Debug, PartialEq, Eq)]
pub enum Token {

    Comment,

    Identifier(Name),

    BoolLiteral(bool),
    NumberLiteral(i32),
    CharLiteral(char),
    StringLiteral(Rc<str>),

    Bool,
    Byte,
    Short,
    Sequence,
    String,
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
    Dot,
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
            "string" => Self::String,
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
    
    fn try_parse_symbol(token: &str) -> Option<Self> {

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
            "." => Self::Dot,
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
            Self::String |
            Self::Stack |
            Self::Array
        )
    }

    pub fn is_specifier_head(&self) -> bool {
        matches!(self,
            Self::At |
            Self::Dot
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

fn lex_line((mut line_num, line): (usize, &str)) -> Result<Vec<Token>, BrainFricError> {

    line_num += 1;

    let mut tokens = Vec::new();
    let mut chars = line.chars().peekable();

    while let Some(char) = chars.peek() {

        if char.is_whitespace() {
            chars.next();
            continue;
        }
        
        if let Some(token) = try_lex_name(&mut chars) {
            tokens.push(token);
            continue;
        }

        match try_lex_symbol(&mut chars) {
            Ok(Some(token)) => {
            
                if token != Token::Comment {
                    tokens.push(token);
                }
            
                continue;
            
            }
            Ok(None) => {}
            Err(()) => err!(line_num, LexError::InvalidSymbol)
        }
        
        if let Some(num) = try_lex_number_literal(&mut chars) {
            tokens.push(Token::NumberLiteral(num));
            continue;
        }

        match try_lex_char_literal(&mut chars) {
            Ok(Some(char_literal)) => {
                tokens.push(Token::CharLiteral(char_literal));
                continue;
            }
            Ok(None) => {}
            Err(()) => err!(line_num, LexError::InvalidCharLiteral)
        }

        match try_lex_string_literal(&mut chars) {
            Ok(Some(string_literal)) => {
                tokens.push(Token::StringLiteral(string_literal));
                continue;
            }
            Ok(None) => {}
            Err(()) => err!(line_num, LexError::InvalidStringLiteral)
        }
        
        err!(line_num, LexError::InvalidToken);

    }

    Ok(tokens)

}

fn try_lex_name(chars: &mut Peekable<Chars>) -> Option<Token> {

    let Some(char) = chars.peek()
    else {
        return None;
    };

    if !(char.is_alphabetic() || *char == '_') {
        return None;
    }

    let mut name_chars = String::new();
    name_chars.push(*char);
    chars.next();

    while let Some(char) = chars.peek() && (char.is_alphanumeric() || *char == '_') {
        name_chars.push(*char);
        chars.next();
    }

    if name_chars.is_empty() {
       None 
    }
    else {
        Token::try_parse_keyword(&name_chars)
            .or_else(|| name_chars.parse::<bool>().map(Token::BoolLiteral).ok())
            .or_else(|| Some(Token::Identifier(name_chars.into())))
    }
}

fn try_lex_symbol(chars: &mut Peekable<Chars>) -> Result<Option<Token>, ()> {

    let mut symbol_chars = String::new();
    let mut last_valid_symbol = None;

    // make function for nameable chars?
    while let Some(char) = chars.peek() && !(char.is_alphanumeric() || matches!(*char, '"' | '\'' | '_')) {
        
        symbol_chars.push(*char);

        if symbol_chars == "//" {
            chars.for_each(drop);
            return Ok(Some(Token::Comment));
        }

        if let Some(symbol) = Token::try_parse_symbol(&symbol_chars) {
            last_valid_symbol = Some(symbol);
        }
        else if last_valid_symbol.is_some() {
            break;
        }
        
        chars.next();
        
    }

    if symbol_chars.is_empty() {
        Ok(None)
    }
    else if last_valid_symbol.is_some() {
        Ok(last_valid_symbol)
    }
    else {
        Err(())
    }
}

fn try_lex_number_literal(chars: &mut Peekable<Chars>) -> Option<i32> {

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

fn try_lex_char_literal(chars: &mut Peekable<Chars>) -> Result<Option<char>, ()> {

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

fn try_lex_string_literal(chars: &mut Peekable<Chars>) -> Result<Option<Rc<str>>, ()> {

    if chars.peek() != Some(&'"') {
        return Ok(None);
    }

    chars.next();

    let mut string_literal = String::new();

    for char in chars {

        if char == '"' {
            return Ok(Some(string_literal.into()));
        }
        
        string_literal.push(char);

    }

    Err(())

}
