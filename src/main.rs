#![feature(let_chains)]
#![feature(if_let_guard)]
#![feature(box_patterns)]
#![feature(exact_size_is_empty)]

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

    if args::arg_show_lex() {
        println!("=== LEXER PASS ===");
        for line in &tokenized {
            println!("{line:?}");
        }
        println!();
    }

    let statements = parse::parse(tokenized, 0)?;

    if args::arg_show_parse() {
        println!("=== PARSER PASS ===");
        print!("{statements:#?}\n");
    }

    let mut ir_generator = ir::IRGenerator::new();
    let mut ir = ir_generator.generate_ir(statements)?;

    if args::arg_show_ir() {
        println!("=== IR PASS ===");
        for ir_statement in &ir {
            println!("{ir_statement:?}");
        }
        println!();
    }

    if args::arg_do_optimization() {
   
        let passes = optimize::optimize(&mut ir);

        if args::arg_show_optimization() {
            println!("=== FINAL OPTIMIZER PASS ({passes} PASSES) ===");
            for ir_statement in &ir {
                println!("{ir_statement:?}");
            }
            println!()
        }
    }

    let mut lowerer = lower::Lowerer::new();
    let mut bf_code = lowerer.lower(ir);

    if args::arg_show_lowered() {
        println!("=== LOWERING PASS ===");
        println!("{bf_code}\n");
    }

    clean::clean(&mut bf_code);

    if args::arg_show_cleaned() {
        println!("=== CLEANING PASS ===");
        println!("{bf_code}\n");
    }

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
