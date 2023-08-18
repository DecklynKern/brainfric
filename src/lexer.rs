pub fn lex(code: &String) -> Vec<Vec<Token>> {
    return code.split("\n").map(lex_line).collect();
}

#[derive(Debug, PartialEq, Eq)]
pub enum Keyword {
    Bool,
    Byte,
    Short,
    Array,
    While,
    End,
    Write,
    Read
}

impl Keyword {
    
    fn try_parse(token: &String) -> Option<Token> {
        Some(Token::Keyword(match token.as_str() {
            "bool" => Self::Bool,
            "byte" => Self::Byte,
            "short" => Self::Short,
            "array" => Self::Array,
            "while" => Self::While,
            "end" => Self::End,
            "write" => Self::Write,
            "read" => Self::Read,
            _ => return None
        }))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Literal {
    Bool(bool),
    Number(usize),
    String(String)
}

impl Literal {
    
    // lol
    fn try_parse_bool(token: &String) -> Option<Token> {
        token.parse::<bool>().map(|literal|Token::Literal(Self::Bool(literal))).ok()
    }
    
    fn try_parse_number(token: &String) -> Option<Token> {
        token.parse::<usize>().map(|literal|Token::Literal(Self::Number(literal))).ok()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Separator {
    OpenParen,
    CloseParen,
    OpenSquare,
    CloseSquare
}

impl Separator {
    
    fn try_parse(token: &String) -> Option<Token> {
        Some(Token::Separator(match token.as_str() {
            "(" => Self::OpenParen,
            ")" => Self::CloseParen,
            "[" => Self::OpenSquare,
            "]" => Self::CloseSquare,
            _ => return None
        }))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Operator {
    SetTo,
    Equals,
    LessThan,
    GreaterThan,
    Plus,
    Minus
}

impl Operator {
    
    fn try_parse(token: &String) -> Option<Token> {

        Some(Token::Operator(match token.as_str() {
            "<-" => Self::SetTo,
            "=" => Self::Equals,
            "<" => Self::LessThan,
            ">" => Self::GreaterThan,
            "+" => Self::Plus,
            "-" => Self::Minus,
            _ => return None
        }))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Identifier(String),
    Keyword(Keyword),
    Literal(Literal),
    Separator(Separator),
    Operator(Operator),
    Comment(String)
}

#[derive(PartialEq, Eq, Debug)]
enum TokenInitialChar {
    Alphabetic,
    Numeric,
    Other,
    None
}

fn lex_line<'a>(line: &str) -> Vec<Token> {

    let mut chars: Vec<char> = line.chars().rev().collect();
    chars.insert(0, ' ');
    
    let mut tokens = Vec::new();
    let mut current_token = String::new();
    let mut current_token_initial_char = TokenInitialChar::None;

    while let Some(chr) = chars.pop() {

        let token_over = chr.is_whitespace();

        if current_token_initial_char == TokenInitialChar::Alphabetic && 
            (!(chr.is_alphanumeric() || chr == '_') || token_over) {

            if let Some(token) = Keyword::try_parse(&current_token) {
                tokens.push(token);
                
            } else if let Some(token) = Literal::try_parse_bool(&current_token) {
                tokens.push(token);
                
            } else {
                tokens.push(Token::Identifier(current_token.clone()));
            }

        } else if current_token_initial_char == TokenInitialChar::Numeric && 
            (!chr.is_numeric() || token_over) {

            if let Some(token) = Literal::try_parse_number(&current_token) {
                tokens.push(token);

            } else {
                todo!()
            }
            
        } else if let Some(token) = Separator::try_parse(&current_token) {
            tokens.push(token);

        } else if let Some(token) = Operator::try_parse(&current_token) {

            let mut try_add = current_token.clone();
            try_add.push(chr);

            if let Some(_) = Operator::try_parse(&try_add) {
                current_token.push(chr);
                continue;
                
            } else {
                tokens.push(token);
            }

        } else if !token_over {
            
            current_token_initial_char = if chr.is_alphabetic() {
                TokenInitialChar::Alphabetic

            } else if chr.is_numeric() {
                TokenInitialChar::Numeric

            } else {
                TokenInitialChar::Other
            };

            current_token.push(chr);
            continue;

        }

        current_token.clear();

        if !chr.is_whitespace() {
            
            current_token_initial_char = if chr.is_alphabetic() {
                TokenInitialChar::Alphabetic

            } else if chr.is_numeric() {
                TokenInitialChar::Numeric

            } else {
                TokenInitialChar::Other
            };

            current_token.push(chr);

        } else {
            current_token_initial_char = TokenInitialChar::None;
        }
    }

    return tokens;

}