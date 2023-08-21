#![feature(let_chains)]
#![feature(if_let_guard)]

use std::fs::File;
use std::io::prelude::*;

use error::BrainFricError;

mod error;
mod lex;
mod parse;
mod compile;

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

    let mut compiler = compile::Compiler::new(statements);
    let compiled = compiler.compile()?;

    println!("=== COMPILER PASS ===");
    println!("{compiled}\n");

    Ok(compiled)

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