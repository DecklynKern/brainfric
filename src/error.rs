use crate::lex::Name;
use crate::parse::DataType;

pub trait ErrorDesc {
    fn get_description(&self) -> String;
}

pub enum LexError {
    InvalidToken(String),
    InvalidCharLiteral(String)
}

impl ErrorDesc for LexError {

    fn get_description(&self) -> String {
        format!("Lex Error: {}", match self {
            Self::InvalidToken(token) => format!("Invalid token: \"{token}\""),
            Self::InvalidCharLiteral(literal) => format!("Invalid char literal '{literal}'")
        })
    }
}

pub enum ParseError {
    InvalidStatement,
    InvalidExpression,
    InvalidAccessor,
    ExpectedOpenAngle,
    ExpectedCloseAngle,
    ExpectedNumberLiteral,
    ExpectedEnd
}

impl ErrorDesc for ParseError {

    fn get_description(&self) -> String {
        format!("Parse Error: {}", match self {
            Self::InvalidStatement => "Invalid statement",
            Self::InvalidExpression => "Invalid expression",
            Self::InvalidAccessor => "Invalid accessor",
            Self::ExpectedOpenAngle => "Expected '<'",
            Self::ExpectedCloseAngle => "Expected '>'",
            Self::ExpectedNumberLiteral => "Expected number literal",
            Self::ExpectedEnd => "Expected end statement to block"
        })
    }
}

pub enum IRError {
    UnknownIdentifier(Name),
    TypeMismatch(DataType, DataType),
    ExpectedReadableAccess,
    ExpectedWriteableAccess,
    ExpectedModifyableAccess,
    ExpectedTypedExpression(DataType)
}

impl ErrorDesc for IRError {

    fn get_description(&self) -> String {
        format!("IR Generation Error: {}", match self {
            Self::UnknownIdentifier(identifier) =>
                format!("Unknown identifier \"{identifier}\""),
            Self::TypeMismatch(expected_type, got_type) =>  
                format!("Type mismatch. Expected {expected_type:?}, got {got_type:?}"),
            Self::ExpectedReadableAccess =>
                "Expected readable access".to_string(),
            Self::ExpectedWriteableAccess =>
                "Expected writeable access".to_string(),
            Self::ExpectedModifyableAccess =>
                "Expected modifyable access".to_string(),
            Self::ExpectedTypedExpression(expected_type) =>
                format!("Expected {expected_type:?} expression")
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