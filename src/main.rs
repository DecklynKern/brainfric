#![feature(let_chains)]
#![feature(if_let_guard)]
#![feature(box_patterns)]

use std::fs::File;
use std::io::prelude::*;

use error::BrainFricError;

mod args;
mod error;
mod lex;
mod parse;
mod ir;
mod optimize;
mod lower;
mod clean;

fn perform_compilation(code: &String) -> Result<String, BrainFricError> {

    let tokenized = lex::lex(code)?;

    println!("=== LEXER PASS ===");
    for line in &tokenized {
        println!("{line:?}");
    }
    println!();

    let statements = parse::parse(tokenized, 0)?;

    println!("=== PARSER PASS ===");
    print!("{statements:#?}\n");

    let mut ir_generator = ir::IRGenerator::new();
    let mut ir = ir_generator.generate_ir(statements)?;

    println!("=== IR PASS ===");
    for ir_statement in &ir {
        println!("{ir_statement:?}");
    }

    if args::arg_do_optimization() {
   
        let passes = optimize::optimize(&mut ir);
        
        println!("\n=== FINAL OPTIMIZER PASS ({passes} PASSES) ===");
        for ir_statement in &ir {
            println!("{ir_statement:?}");
        }
    }

    let mut lowerer = lower::Lowerer::new();
    let mut bf_code = lowerer.lower(ir);

    clean::clean(&mut bf_code);

    Ok(bf_code)

}

fn main() -> std::io::Result<()> {

    args::parse_args();

    let mut file = File::open("in.bfrc")?;

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
