extern crate brainfric;

use brainfric::lex::*;
use brainfric::parse::*;
use brainfric::ir::*;
use brainfric::optimize::*;
use brainfric::lower::*;

macro_rules! program {
    () => {
        "
        byte a
        byte b
        byte c

        a <- 240
        read b

        c <- a - b
        write c
        "
    }
}
macro_rules! tokens {
    () => {
        vec![
            vec![],
            vec![
                Token::Type(Type::Byte),
                Token::Identifier("a".into())
            ],
            vec![
                Token::Type(Type::Byte),
                Token::Identifier("b".into())
            ],
            vec![
                Token::Type(Type::Byte),
                Token::Identifier("c".into())
            ],
            vec![],
            vec![
                Token::Identifier("a".into()),
                Token::BinaryOperator(BinaryOperator::SetTo),
                Token::Literal(Literal::Number(240))
            ],
            vec![
                Token::Keyword(Keyword::Read),
                Token::Identifier("b".into())
            ],
            vec![],
            vec![
                Token::Identifier("c".into()),
                Token::BinaryOperator(BinaryOperator::SetTo),
                Token::Identifier("a".into()),
                Token::BinaryOperator(BinaryOperator::Minus),
                Token::Identifier("b".into())
            ],
            vec![
                Token::Keyword(Keyword::Write),
                Token::Identifier("c".into())
            ],
            vec![],
        ]
    };
}
macro_rules! statements {
    () => {
        vec![
            Statement {
                line_num: 2,
                body: StatementBody::Declaration("a".into(), DataType::Byte)
            },
            Statement {
                line_num: 3,
                body: StatementBody::Declaration("b".into(), DataType::Byte)
            },
            Statement {
                line_num: 4,
                body: StatementBody::Declaration("c".into(), DataType::Byte)
            },
            Statement {
                line_num: 6,
                body: StatementBody::SetTo(
                    "a".into(),
                    Expression::NumberLiteral(240)
                )
            },
            Statement {
                line_num: 7,
                body: StatementBody::Read("b".into())
            },
            Statement {
                line_num: 9,
                body: StatementBody::SetTo(
                    "c".into(),
                    Expression::Subtract(
                        Box::new(Expression::Identifier("a".into())),
                        Box::new(Expression::Identifier("b".into()))
                    )
                )
            },
            Statement {
                line_num: 10,
                body: StatementBody::Write(Expression::Identifier("c".into()))
            }
        ]
    };
}

macro_rules! bf_code {
    () => {
        ">,<---------------->>----------------<[->>+<<]>>[-<<->->]<."
    };
}

#[test]
fn lexer_check() {
    assert_eq!(
        lex(program!()).unwrap_or_else(|_| panic!()),
        tokens!()
    )
}

#[test]
fn parser_check() {
    assert_eq!(
        parse(tokens!(), 0).unwrap_or_else(|_| panic!()),
        statements!()
    )
}

// #[test]
// fn basic_ir_check() {

//     let mut ir_generator = IRGenerator::new();
//     let mut generated_ir = ir_generator.generate_ir(statements!()).unwrap_or_else(|_| panic!());
    
//     optimize(&mut generated_ir);

//     assert_eq!(
//         generated_ir,
//         ir_statements!()
// //     )
// // }

// #[test]
// fn basic_lower_check() {

//     let mut lowerer = Lowerer::new();

//     assert_eq!(
//         lowerer.lower(ir_statements!().0),
//         bf_code!()
//     )
// }

#[test]
fn ir_to_lowered_check() {

    let mut generated_ir = generate_ir(statements!()).unwrap_or_else(|_| panic!());
    
    optimize(&mut generated_ir);

    assert_eq!(
        lower(generated_ir.0),
        bf_code!()
    )
}