use crate::{
    lexer::Lexer,
    token::{Token, TokenKind},
};

pub mod ast;

#[derive(PartialEq, Debug)]
pub enum Precedence {
    Lowest,
    Equal,       // ==
    LessGreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // myFunction(X)
}

impl Precedence {
    fn from_token(kind: &TokenKind) -> Precedence {
        match kind {
            TokenKind::Illegal => todo!(),
            TokenKind::EOF => todo!(),
            TokenKind::Identifier => todo!(),
            TokenKind::Int => todo!(),
            TokenKind::Assign => todo!(),
            TokenKind::Plus => todo!(),
            TokenKind::Comma => todo!(),
            TokenKind::Semicolon => todo!(),
            TokenKind::LParen => todo!(),
            TokenKind::RParen => todo!(),
            TokenKind::LBrace => todo!(),
            TokenKind::RBrace => todo!(),
            TokenKind::Function => todo!(),
            TokenKind::Let => todo!(),
            TokenKind::Minus => todo!(),
            TokenKind::Bang => todo!(),
            TokenKind::Asterisk => todo!(),
            TokenKind::Slash => todo!(),
            TokenKind::Less => todo!(),
            TokenKind::Greater => todo!(),
            TokenKind::LessEqual => todo!(),
            TokenKind::GreaterEqual => todo!(),
            TokenKind::True => todo!(),
            TokenKind::False => todo!(),
            TokenKind::If => todo!(),
            TokenKind::Else => todo!(),
            TokenKind::Return => todo!(),
            TokenKind::Equal => todo!(),
            TokenKind::NotEqual => todo!(),
        }
    }
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            lexer: Lexer::new(input),
        }
    }
}
