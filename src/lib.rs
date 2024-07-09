#![feature(let_chains)]
#![feature(if_let_guard)]
#![feature(box_patterns)]
#![feature(exact_size_is_empty)]

pub mod args;
pub mod error;
pub mod lex;
pub mod parse;
pub mod elaborate;
pub mod ir;
pub mod optimize;
pub mod lower;