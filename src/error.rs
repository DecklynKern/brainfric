use std::fmt::*;

pub trait BrainFricError {
    fn get_description(&self) -> String;
}

pub enum LexerError {
    InvalidToken(String)
}

impl Debug for LexerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.get_description())
    }
}

impl BrainFricError for LexerError {

    fn get_description(&self) -> String {
        format!("Lexer Error: {}", match self {
            Self::InvalidToken(token) => format!("Invalid token: \"{token}\"")
        })
    }
}

pub enum ParserError {
    InvalidStatement
}

impl BrainFricError for ParserError {

    fn get_description(&self) -> String {
        format!("Parser Error: {}", match self {
            Self::InvalidStatement => "Invalid statement"
        })
    }
}

impl Debug for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.get_description())
    }
}

pub enum CompilerError {
    UnknownIdentifier(String)
}

impl BrainFricError for CompilerError {

    fn get_description(&self) -> String {
        format!("Compiler Error: {}", match self {
            Self::UnknownIdentifier(identifier) => format!("Unknown identifier: \"{identifier}\"")
        })
    }
}

impl Debug for CompilerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.get_description())
    }
}