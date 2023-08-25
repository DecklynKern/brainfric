extern crate brainfric;

use brainfric::*;

#[test]
fn basic_lexer_check() {
    assert_eq!(
        lex::lex(&"   byte     a   \n  a  <-  240  ".to_string()).unwrap_or_else(|_| panic!()),
        vec![
            vec![
                lex::Token::Keyword(lex::Keyword::Byte),
                lex::Token::Identifier("a".to_string())
            ],
            vec![
                lex::Token::Identifier("a".to_string()),
                lex::Token::Operator(lex::Operator::SetTo),
                lex::Token::Literal(lex::Literal::Number(240))
            ]
        ]
    )
}

#[test]
fn basic_parser_check() {

    let mut parser = parse::Parser::new(vec![
        vec![
            lex::Token::Keyword(lex::Keyword::Byte),
            lex::Token::Identifier("a".to_string())
        ],
        vec![
            lex::Token::Identifier("a".to_string()),
            lex::Token::Operator(lex::Operator::SetTo),
            lex::Token::Literal(lex::Literal::Number(240))
        ]
    ]);

    assert_eq!(
        parser.parse().unwrap_or_else(|_| panic!()),
        vec![
            (0, parse::Statement::Declaration("a".to_string(), parse::DataType::Byte)),
            (1, parse::Statement::SetTo(
                "a".to_string(),
                parse::Expression::NumberLiteral(240)
            ))
        ]
    )
}