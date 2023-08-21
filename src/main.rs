#![feature(let_chains)]
#![feature(if_let_guard)]

use std::fs::File;
use std::io::prelude::*;

use error::BrainFricError;

mod error;
mod lexer;
mod parser;
mod compiler;

fn perform_compilation(code: &String) -> Result<String, Box<dyn BrainFricError>> {

    let tokenized = match lexer::lex(code) {
        Ok(tokenized) => tokenized,
        Err(err) => return Err(Box::new(err))
    };

    println!("=== LEXER PASS ===");
    for line in &tokenized {
        println!("{line:?}");
    }
    println!();

    let statements = match parser::parse(tokenized) {
        Ok(statements) => statements,
        Err(err) => return Err(Box::new(err))
    };

    println!("=== PARSER PASS ===");
    for statement in &statements {
        println!("{statement:?}");
    }
    println!();

    let mut main_compiler = compiler::Compiler::new(statements);

    let compiled = match main_compiler.compile() {
        Ok(compiled) => compiled,
        Err(err) => return Err(Box::new(err))
    };

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
            println!("{}", err.get_description());
            return Ok(());
        }
    };

    let mut file = File::create("out.bf")?;
    file.write_all(compiled.as_bytes())?;

    Ok(())
    
}