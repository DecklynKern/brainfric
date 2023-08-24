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
    ExpectedNumberLiteral

}

impl ErrorDesc for ParseError {

    fn get_description(&self) -> String {
        format!("Parse Error: {}", match self {
            Self::InvalidStatement => format!("Invalid statement"),
            Self::InvalidExpression => format!("Invalid expression"),
            Self::ExpectedIdentifier => format!("Expected identifier"),
            Self::ExpectedNumberLiteral => format!("Expected number literal")
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

pub enum CompileError {
    UnknownIdentifier(String),
    NumberLiteralTooLarge(u32),
    IncorrectExpressionType(DataType, DataType),
    IncorrectVariableType(DataType, DataType),
    TypeMismatch(DataType, DataType)
}

impl ErrorDesc for CompileError {

    fn get_description(&self) -> String {
        format!("Compile Error: {}", match self {
            Self::UnknownIdentifier(identifier) =>
                format!("Unknown identifier \"{identifier}\""),
            Self::NumberLiteralTooLarge(literal) =>
                format!("Number literal \"{}\" too large", literal),
            Self::IncorrectExpressionType(expected_type, got_type) =>
                format!("Incorrect expression type. Expected {:?} got {:?}", expected_type, got_type),
            Self::IncorrectVariableType(expected_type, got_type) =>
                format!("Incorrect variable type. Expected {:?} got {:?}", expected_type, got_type),
            Self::TypeMismatch(var_type, expr_type) =>  
                format!("Type mismatch {:?} {:?}", var_type, expr_type)
        })
    }
}

pub enum InternalCompilerError {
    UnfreedRegister,
    FreeVariableAsRegister,
    BadRegisterFreeOrder
}

impl ErrorDesc for InternalCompilerError {

    fn get_description(&self) -> String {
        format!("Compiler Error: {}", match self {
            Self::UnfreedRegister => "Unfreed register by the time of current instruction",
            Self::FreeVariableAsRegister => "Attempted to free variable as register",
            Self::BadRegisterFreeOrder => "Register attempted to be freed in incorrect order"
        })
    }
}

pub struct BrainFricError {
    pub line: usize,
    pub error: Box<dyn ErrorDesc>
}

impl BrainFricError {
    
    pub fn print(&self) {
        println!("Error on line {}.", self.line);
        println!("{}", self.error.get_description());
    }
}

#[macro_export]
macro_rules! err {
    ($line_num:expr, $error:expr) => {
        return Err(BrainFricError {
            line: $line_num + 1,
            error: Box::new($error)
        })
    }
}