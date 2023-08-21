extern crate brainfric;

use brainfric::*;

#[test]
fn basic_lexer_check() {
    assert_eq!(
        lexer::lex(&"   byte     a   \n  a  <-  240  ".to_string()).unwrap(),
        vec![
            vec![
                lexer::Token::Keyword(lexer::Keyword::Byte),
                lexer::Token::Identifier("a".to_string())
            ],
            vec![
                lexer::Token::Identifier("a".to_string()),
                lexer::Token::Operator(lexer::Operator::SetTo),
                lexer::Token::Literal(lexer::Literal::Number(240))
            ]
        ]
    )
}

#[test]
fn basic_parser_check() {
    assert_eq!(
        parser::parse(vec![
            vec![
                lexer::Token::Keyword(lexer::Keyword::Byte),
                lexer::Token::Identifier("a".to_string())
            ],
            vec![
                lexer::Token::Identifier("a".to_string()),
                lexer::Token::Operator(lexer::Operator::SetTo),
                lexer::Token::Literal(lexer::Literal::Number(240))
            ]
        ]).unwrap(),
        vec![
            parser::Statement::Declaration("a".to_string(), parser::DataType::Byte),
            parser::Statement::SetTo(
                "a".to_string(),
                parser::Expression::NumberLiteral(240)
            )
        ]
    )
}

#[test]
fn basic_compiler_check() {

    let mut main_compiler = compiler::Compiler::new(vec![
        parser::Statement::Declaration("a".to_string(), parser::DataType::Byte),
        parser::Statement::SetTo(
            "a".to_string(),
            parser::Expression::NumberLiteral(240)
        )
    ]);

    assert_eq!(
        main_compiler.compile().unwrap(),
        "----------------".to_string()
    );
}