const PROMPT: &str = "Rust >> ";

use crate::lexer::Lexer;
use crate::token::TokenType;
use anyhow::Result;
use std::io::{stdin, stdout, Write};

pub struct Repl;

impl Repl {
    pub fn start() -> Result<()> {
        let mut buf = String::new();
        loop {
            print!("\n{}", PROMPT);
            stdout().flush()?;
            stdin().read_line(&mut buf)?;
            let mut l = Lexer::new(&buf);
            let mut t = l.next_token();
            while t.typ != TokenType::EOF {
                println!("{:?}", t);
                t = l.next_token();
            }
            buf.clear();
        }
    }
}
