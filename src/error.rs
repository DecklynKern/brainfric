use crate::lex::Name;
use crate::ir::DataType;

pub trait ErrorDesc {
    fn get_description(&self) -> String;
}

pub enum LexError {
    InvalidToken,
    InvalidSymbol,
    InvalidStringLiteral,
    InvalidCharLiteral
}

impl ErrorDesc for LexError {

    fn get_description(&self) -> String {
        format!("Lex Error: {}", match self {
            Self::InvalidToken => "Invalid token",
            Self::InvalidSymbol => "Invalid symbol",
            Self::InvalidStringLiteral => "Invalid string literal",
            Self::InvalidCharLiteral => "Invalid char literal"
        })
    }
}

pub enum ParseError {
    InvalidStatement,
    InvalidExpression,
    InvalidAccessor,
    ExpectedOpenAngle,
    ExpectedCloseAngle,
    ExpectedCloseParen,
    ExpectedNumberLiteral,
    ExpectedEnd,
    ExpectedNewline,
    StringLiteralTooLarge,
    MultipleDefaultArms
}

impl ErrorDesc for ParseError {

    fn get_description(&self) -> String {
        format!("Parse Error: {}", match self {
            Self::InvalidStatement => "Invalid statement",
            Self::InvalidExpression => "Invalid expression",
            Self::InvalidAccessor => "Invalid accessor",
            Self::ExpectedOpenAngle => "Expected '<'",
            Self::ExpectedCloseAngle => "Expected '>'",
            Self::ExpectedCloseParen => "Expected ')'",
            Self::ExpectedNumberLiteral => "Expected number literal",
            Self::ExpectedEnd => "Expected end statement to block",
            Self::ExpectedNewline => "Expected new line",
            Self::StringLiteralTooLarge => "String literal too large for declared sequence",
            Self::MultipleDefaultArms => "Multiple default arms"
        })
    }
}

pub enum IRError {
    UnknownIdentifier(Name),
    TypeMismatch(DataType, DataType),
    ExpectedSequence,
    ExpectedTypedExpression(DataType),
    OutOfBoundsAccess,
    ShiftTooLarge,
    InvalidTypeParameters
}

impl ErrorDesc for IRError {

    fn get_description(&self) -> String {
        format!("IR Generation Error: {}", match self {
            Self::UnknownIdentifier(identifier) =>
                format!("Unknown identifier \"{identifier}\""),
            Self::TypeMismatch(expected_type, got_type) =>  
                format!("Type mismatch. Expected {expected_type:?}, got {got_type:?}"),
            Self::ExpectedSequence =>
                "Expected sequence".to_string(),
            Self::ExpectedTypedExpression(expected_type) =>
                format!("Expected {expected_type:?} expression"),
            Self::OutOfBoundsAccess =>
                "Out of bounds access".to_string(),
            Self::ShiftTooLarge =>
                "Shift is too large for sequence size".to_string(),
            Self::InvalidTypeParameters =>
                "Invalid type parameters".to_string()
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
            println!("Internal compiler error.")
        }

        println!("{}", self.error.get_description());
    }
}

#[macro_export]
macro_rules! err {
    ($line_num: expr, $error: expr) => {
        return Err(BrainFricError {
            line: $line_num,
            error: Box::new($error)
        })
    }
}
