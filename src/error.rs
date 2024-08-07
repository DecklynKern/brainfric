use std::rc::Rc;

use crate::lex::Name;
use crate::elaborate::ElaboratedDataType;

static mut CURRENT_LINE_NUM: usize = 0;

pub fn set_line_num(line_num: usize) {
    unsafe {
        CURRENT_LINE_NUM = line_num;
    }
}

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
    InvalidType,
    InvalidDefinition,
    ExpectedOpenAngle,
    ExpectedCloseParen,
    ExpectedCloseSquare,
    ExpectedCloseAngle,
    ExpectedEqual,
    ExpectedNumberLiteral,
    ExpectedMatchArm,
    ExpectedName,
    ExpectedEnd,
    ExpectedNewline,
    ExpectedIdentifier,
    MultipleDefaultArms
}

impl ErrorDesc for ParseError {

    fn get_description(&self) -> String {
        format!("Parse Error: {}", match self {
            Self::InvalidStatement => "Invalid statement",
            Self::InvalidExpression => "Invalid expression",
            Self::InvalidAccessor => "Invalid accessor",
            Self::InvalidType => "Invalid type",
            Self::InvalidDefinition => "Invalid definition",
            Self::ExpectedOpenAngle => "Expected '<'",
            Self::ExpectedCloseParen => "Expected ')'",
            Self::ExpectedCloseSquare => "Expected ']'",
            Self::ExpectedCloseAngle => "Expected '>'",
            Self::ExpectedEqual => "Expected '='",
            Self::ExpectedNumberLiteral => "Expected number literal",
            Self::ExpectedMatchArm => "Expected match arm",
            Self::ExpectedName => "Expected name",
            Self::ExpectedEnd => "Expected end statement to block",
            Self::ExpectedNewline => "Expected new line",
            Self::ExpectedIdentifier => "Expected identifier",
            Self::MultipleDefaultArms => "Multiple default arms"
        })
    }
}

pub enum ElaborateError {
    UnknownIdentifier(Name),
    UnknownType(Name),
    UnknownEnum(Name),
    UnknownEnumVariant(Name, Name),
    UnknownMacro(Name),
    StringLiteralTooLarge(Rc<str>, usize),
    TypeMismatch(ElaboratedDataType, ElaboratedDataType),
    NestedComplexType,
    ExpectedSequence,
    ExpectedTypedExpression(ElaboratedDataType),
    OutOfBoundsAccess,
    ShiftTooLarge,
    InvalidTypeParameters,
    MismatchedArgumentCount
}

impl ErrorDesc for ElaborateError {

    fn get_description(&self) -> String {
        format!("Elaboration Generation Error: {}", match self {
            Self::UnknownIdentifier(identifier) =>
                format!("Unknown identifier \"{identifier}\""),
            Self::UnknownType(type_name) =>
                format!("Unknown type \"{type_name}\""),
            Self::UnknownEnum(enum_name) => 
                format!("Unknown enum \"{enum_name}\""),
            Self::UnknownEnumVariant(enum_name, variant_name) => 
                format!("Unknown enum variant \"{enum_name}::{variant_name}\""),
            Self::UnknownMacro(macro_name) => 
                format!("Unknown macro \"{macro_name}\""),
            Self::StringLiteralTooLarge(literal, max_size) =>
                format!("String literal too large \"{literal}\", max size: {max_size}"),
            Self::TypeMismatch(expected_type, got_type) =>  
                format!("Type mismatch. Expected {expected_type:?}, got {got_type:?}"),
            Self::NestedComplexType =>
                "Nested complex types are currently unsupported".to_string(),
            Self::ExpectedSequence =>
                "Expected sequence".to_string(),
            Self::ExpectedTypedExpression(expected_type) =>
                format!("Expected {expected_type:?} expression"),
            Self::OutOfBoundsAccess =>
                "Out of bounds access".to_string(),
            Self::ShiftTooLarge =>
                "Shift is too large for sequence size".to_string(),
            Self::InvalidTypeParameters =>
                "Invalid type parameters".to_string(),
            Self::MismatchedArgumentCount =>
                "Wrong argument count provided for macro".to_string()
        })
    }
}

pub struct BrainFricError {
    pub line: usize,
    pub error: Box<dyn ErrorDesc>
}

impl BrainFricError {

    pub fn new(error: Box<dyn ErrorDesc>) -> Self {
        Self {
            line: unsafe {CURRENT_LINE_NUM},
            error
        }
    }
    
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

pub type MaybeBFErr<Type> = Result<Type, BrainFricError>;

#[macro_export]
macro_rules! err {
    ($error: expr) => {
        return Err(BrainFricError::new(Box::new($error)))
    }
}
