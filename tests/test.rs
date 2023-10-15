extern crate brainfric;

use brainfric::lex::*;
use brainfric::parse::*;
use brainfric::ir::*;
use brainfric::optimize::*;
use brainfric::lower::*;

macro_rules! program {
    () => {
        "
        byte a, b, c

        a <- 240
        read b

        c <- a - b
        write c
        "
    }
}

macro_rules! id {
    ($name: literal) => {
        Token::Identifier($name.into())
    }
}
macro_rules! tokens {
    () => {
        vec![
            vec![],
            vec![
                Token::Byte,
                id!("a"),
                Token::Comma,
                id!("b"),
                Token::Comma,
                id!("c")
            ],
            vec![],
            vec![
                id!("a"),
                Token::SetTo,
                Token::NumberLiteral(240)
            ],
            vec![
                Token::Read,
                id!("b")
            ],
            vec![],
            vec![
                id!("c"),
                Token::SetTo,
                id!("a"),
                Token::Hypen,
                id!("b")
            ],
            vec![
                Token::Write,
                id!("c")
            ],
            vec![],
        ]
    };
}

macro_rules! access {
    ($name: literal) => {
        Accessor::from_name($name.into())
    }
}
macro_rules! statements {
    () => {
        vec![
            Statement {
                line_num: 2,
                body: StatementBody::Declaration(vec!["a".into(), "b".into(), "c".into()], DataType::Byte)
            },
            Statement {
                line_num: 4,
                body: StatementBody::SetTo(
                    access!("a"),
                    Expression::NumberLiteral(240)
                )
            },
            Statement {
                line_num: 5,
                body: StatementBody::Read(access!("b"))
            },
            Statement {
                line_num: 7,
                body: StatementBody::SetTo(
                    access!("c"),
                    Expression::Subtract(
                        Box::new(Expression::Access(access!("a"))),
                        Box::new(Expression::Access(access!("b")))
                    )
                )
            },
            Statement {
                line_num: 8,
                body: StatementBody::Write(Expression::Access(access!("c")))
            }
        ]
    };
}

macro_rules! bf_code {
    () => {
        ">,<---------------->>----------------<[->>+<+<]>>[-<<->>]<."
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