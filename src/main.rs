#![feature(let_chains)]
#![feature(if_let_guard)]

use std::fs::File;
use std::io::prelude::*;

use error::BrainFricError;

mod error;
mod lexer;
mod parser;
mod compiler;

fn perform_compilation(code: &String) -> Result<String, BrainFricError> {

    let tokenized = lexer::lex(code)?;

    println!("=== LEXER PASS ===");
    for line in &tokenized {
        println!("{line:?}");
    }
    println!();

    let statements = parser::parse(tokenized)?;

    println!("=== PARSER PASS ===");
    for statement in &statements {
        println!("{statement:?}");
    }
    println!();

    let mut main_compiler = compiler::Compiler::new(statements);

    let compiled = main_compiler.compile()?;

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