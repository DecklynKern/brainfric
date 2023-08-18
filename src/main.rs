#![feature(let_chains)]
#![feature(if_let_guard)]

use std::fs::File;
use std::io::prelude::*;

mod lexer;
mod parser;
mod compiler;

fn main() -> std::io::Result<()> {

    let mut file = File::open("bf")?;

    let mut code = String::new();
    file.read_to_string(&mut code)?;

    let tokenized = lexer::lex(&code);

    println!("=== LEXER PASS ===");
    for line in &tokenized {
        println!("{line:?}");
    }

    let statements = parser::parse(tokenized);

    println!("\n=== PARSER PASS ===");
    for statement in &statements {
        println!("{statement:?}");
    }

    let compiled = compiler::compile(statements);

    println!("\n=== COMPILER PASS ===");
    println!("{compiled}");

    let mut file = File::create("out")?;
    file.write_all(compiled.as_bytes())?;

    Ok(())
    
}