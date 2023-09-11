extern crate brainfric;

use brainfric::*;

macro_rules! program {
    () => {
        "   byte     a   \n  a  <-  240  "
    }
}
macro_rules! tokens {
    () => {
        vec![
            vec![
                lex::Token::Type(lex::Type::Byte),
                lex::Token::Identifier("a".to_string())
            ],
            vec![
                lex::Token::Identifier("a".to_string()),
                lex::Token::BinaryOperator(lex::BinaryOperator::SetTo),
                lex::Token::Literal(lex::Literal::Number(240))
            ]
        ]
    };
}
macro_rules! statements {
    () => {
        vec![
            parse::Statement {
                line_num: 1,
                body: parse::StatementBody::Declaration("a".to_string(), parse::DataType::Byte)
            },
            parse::Statement {
                line_num: 2,
                body: parse::StatementBody::SetTo(
                    "a".to_string(),
                    parse::Expression::NumberLiteral(240)
                )
            }
        ]
    };
}

macro_rules! ir_statements {
    () => {
        vec![
            ir::IRStatement::Alloc(0, 1, false),
            ir::IRStatement::AddConst(0, 240)
        ]
    };
}

macro_rules! bf_code {
    () => {
        "----------------"
    };
}

#[test]
fn basic_lexer_check() {
    assert_eq!(
        lex::lex(program!()).unwrap_or_else(|_| panic!()),
        tokens!()
    )
}

#[test]
fn basic_parser_check() {
    assert_eq!(
        parse::parse(tokens!(), 0).unwrap_or_else(|_| panic!()),
        statements!()
    )
}

#[test]
fn basic_ir_check() {

    let mut ir_generator = ir::IRGenerator::new();
    let mut generated_ir = ir_generator.generate_ir(statements!()).unwrap_or_else(|_| panic!());
    
    optimize::optimize(&mut generated_ir);

    assert_eq!(
        generated_ir,
        ir_statements!()
    )
}

#[test]
fn basic_lower_check() {

    let mut lowerer = lower::Lowerer::new();

    assert_eq!(
        lowerer.lower(ir_statements!()),
        bf_code!()
    )
}