#![feature(let_chains)]
#![feature(if_let_guard)]

pub mod args;
pub mod error;
pub mod lex;
pub mod parse;
pub mod ir;
pub mod optimize;
pub mod lower;