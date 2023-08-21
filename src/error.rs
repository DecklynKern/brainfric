pub trait ErrorDesc {
    fn get_description(&self) -> String;
}

pub enum LexerError {
    InvalidToken(String)
}

impl ErrorDesc for LexerError {

    fn get_description(&self) -> String {
        format!("Lexer Error: {}", match self {
            Self::InvalidToken(token) => format!("Invalid token: \"{token}\"")
        })
    }
}

pub enum ParserError {
    InvalidStatement,
    InvalidExpression,
    ExpectedIdentifier
}

impl ErrorDesc for ParserError {

    fn get_description(&self) -> String {
        format!("Parser Error: {}", match self {
            Self::InvalidStatement => format!("Invalid statement"),
            Self::InvalidExpression => format!("Invalid expression"),
            Self::ExpectedIdentifier => format!("Expected identifier")
        })
    }
}

pub enum CompilerError {
    UnknownIdentifier(String)
}

impl ErrorDesc for CompilerError {

    fn get_description(&self) -> String {
        format!("Compiler Error: {}", match self {
            Self::UnknownIdentifier(identifier) => format!("Unknown identifier: \"{identifier}\"")
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