use crate::parse::DataType;

pub trait ErrorDesc {
    fn get_description(&self) -> String;
}

pub enum LexError {
    InvalidToken(String)
}

impl ErrorDesc for LexError {

    fn get_description(&self) -> String {
        format!("Lex Error: {}", match self {
            Self::InvalidToken(token) => format!("Invalid token: \"{token}\"")
        })
    }
}

pub enum ParseError {
    InvalidStatement,
    InvalidExpression,
    ExpectedIdentifier,
    ExpectedNumberLiteral,
    ExpectedEnd
}

impl ErrorDesc for ParseError {

    fn get_description(&self) -> String {
        format!("Parse Error: {}", match self {
            Self::InvalidStatement => format!("Invalid statement"),
            Self::InvalidExpression => format!("Invalid expression"),
            Self::ExpectedIdentifier => format!("Expected identifier"),
            Self::ExpectedNumberLiteral => format!("Expected number literal"),
            Self::ExpectedEnd => format!("Expected end statement to block")
        })
    }
}

pub enum IRError {
    UnknownIdentifier(String),
    TypeMismatch(DataType, DataType)
}

impl ErrorDesc for IRError {

    fn get_description(&self) -> String {
        format!("IR Generation Error: {}", match self {
            Self::UnknownIdentifier(identifier) =>
                format!("Unknown identifier \"{identifier}\""),
            Self::TypeMismatch(expected_type, got_type) =>  
                format!("Type mismatch. Expected {:?}, got {:?}", expected_type, got_type)
        })
    }
}

pub struct BrainFricError {
    pub line: usize,
    pub error: Box<dyn ErrorDesc>
}

impl BrainFricError {
    
    pub fn print(&self) {

        if self.line != 0 {
            println!("Error on line {}.", self.line);
        }
        else {
            println!("Interal compiler error.")
        }

        println!("{}", self.error.get_description());
    }
}

#[macro_export]
macro_rules! err {
    ($line_num:expr, $error:expr) => {
        return Err(BrainFricError {
            line: $line_num,
            error: Box::new($error)
        })
    }
}