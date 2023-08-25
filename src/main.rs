#![feature(let_chains)]
#![feature(if_let_guard)]

use std::fs::File;
use std::io::prelude::*;

use error::BrainFricError;

mod error;
mod lex;
mod parse;
mod ir;
mod optimize;
mod lower;

fn perform_compilation(code: &String) -> Result<String, BrainFricError> {

    let tokenized = lex::lex(code)?;

    println!("=== LEXER PASS ===");
    for line in &tokenized {
        println!("{line:?}");
    }
    println!();

    let mut parser = parse::Parser::new(tokenized);
    let statements = parser.parse()?;

    println!("=== PARSER PASS ===");
    for statement in &statements {
        println!("{statement:?}");
    }
    println!();

    let mut ir_generator = ir::IRGenerator::new();
    let mut ir = ir_generator.generate_ir(statements)?;

    println!("=== IR PASS ===");
    for ir_statement in &ir {
        println!("{ir_statement:?}");
    }

    let pass = 0;
    
    while optimize::optimize(&mut ir) {

        pass += 1;

        println!("\n=== OPTIMIZER PASS {pass} ===");
        for ir_statement in &ir {
            println!("{ir_statement:?}");
        }
    }

    let mut lowerer = lower::Lowerer::new();
    Ok(lowerer.lower(ir))

}

fn main() -> std::io::Result<()> {

    let mut file = File::open("bf")?;

    let mut code = String::new();
    file.read_to_string(&mut code)?;

    let compiled = match perform_compilation(&code) {
        Ok(compiled) => compiled,
        Err(err) => {
            err.print();
            return Ok(());
        }
    };

    let mut file = File::create("out.bf")?;
    file.write_all(compiled.as_bytes())?;

    Ok(())
    
}
