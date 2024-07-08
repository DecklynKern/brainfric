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
mod elaborate;
//mod ir;
//mod optimize;
//mod lower;
//mod clean;

fn compile(code: &str) -> Result<String, BrainFricError> {

    let tokenized = lex::lex(code)?;

    if args::arg_show_lex() {
        println!("=== LEXER PASS ===");
        for line in &tokenized {
            println!("{line:?}");
        }
        println!();
    }

    let parsed_program = parse::parse(&tokenized)?;

    if args::arg_show_parse() {
        println!("=== PARSER PASS ===");

        println!("Definitions:");
        for definition in &parsed_program.definitions {
            println!("{definition:?}");
        }

        println!("Code:");
        for statement in &parsed_program.code {
            println!("{statement:?}");
        }
    }

    let elaborated_program = elaborate::elaborate(parsed_program)?;

    // let mut ir = ir::generate_ir(elaborated_program)?;

    // if args::arg_show_ir() {
    //     println!("=== IR PASS ===");
    //     for ir_statement in &ir.0 {
    //         println!("{ir_statement:?}");
    //     }
    //     println!();
    // }

    // if args::arg_do_optimization() {
   
    //     let passes = optimize::optimize(&mut ir);

    //     if args::arg_show_optimization() {

    //         println!(
    //             "=== FINAL OPTIMIZER PASS ({} STAGE 1 PASSES, {} STAGE 2 PASSES) ===",
    //             passes[0],
    //             passes[1]
    //         );

    //         for ir_statement in &ir.0 {
    //             println!("{ir_statement:?}");
    //         }
    //         println!();

    //     }
    // }

    // let mut bf_code = lower::lower(ir);

    // if args::arg_show_lowered() {
    //     println!("=== LOWERING PASS ===");
    //     println!("{bf_code}\n");
    // }

    // clean::clean(&mut bf_code);

    // if args::arg_show_cleaned() {
    //     println!("=== CLEANING PASS ===");
    //     println!("{bf_code}\n");
    // }

    // Ok(bf_code)

    Ok(String::from(""))

}

fn main() -> std::io::Result<()> {

    args::parse_args();

    let mut file = File::open(args::arg_input_file())?;

    let mut code = String::new();
    file.read_to_string(&mut code)?;

    let compiled = match compile(&code) {
        Ok(compiled) => compiled,
        Err(err) => {
            err.print();
            return Ok(());
        }
    };

    let mut file = File::create(args::arg_output_file())?;
    file.write_all(compiled.as_bytes())?;

    Ok(())
    
}
