const PROMPT: &str = "Rust >> ";

use crate::parser::Parser;
use crate::token::TokenKind;
use crate::{eval::Evaluator, lexer::Lexer};
use anyhow::Result;
use std::io::{stdin, stdout, Write};

pub struct Repl;

impl Repl {
    pub fn start() -> Result<()> {
        let mut eval = Evaluator::new();
        let mut buf = String::new();
        loop {
            print!("\n{}", PROMPT);
            stdout().flush()?;
            stdin().read_line(&mut buf)?;
            match Parser::new(&buf).parse_program() {
                Ok(p) => match eval.eval_block_stmt(p) {
                    Ok(ret) => println!("{ret}"),
                    Err(e) => println!("{e:?}"),
                },
                Err(e) => println!("{e:?}"),
            };
            buf.clear();
        }
    }
}
