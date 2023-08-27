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

    assert_eq!(
        parse::parse(vec![
            vec![
                lex::Token::Keyword(lex::Keyword::Byte),
                lex::Token::Identifier("a".to_string())
            ],
            vec![
                lex::Token::Identifier("a".to_string()),
                lex::Token::Operator(lex::Operator::SetTo),
                lex::Token::Literal(lex::Literal::Number(240))
            ]
        ], 0).unwrap_or_else(|_| panic!()),
        vec![
            (1, parse::Statement::Declaration("a".to_string(), parse::DataType::Byte)),
            (2, parse::Statement::SetTo(
                "a".to_string(),
                parse::Expression::NumberLiteral(240)
            ))
        ]
    )
}
