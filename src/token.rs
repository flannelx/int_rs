/*
    ILLEGAL = "ILLEGAL"
    EOF     = "EOF"

    // Identifiers + literals
    IDENT = "IDENT" // add, foobar, x, y, ...
    INT   = "INT"   // 1343456

    // Operators
    ASSIGN   = "="
    PLUS     = "+"
    MINUS    = "-"
    BANG     = "!"
    ASTERISK = "*"
    SLASH    = "/"

    LT = "<"
    GT = ">"

    // Delimiters
    COMMA     = ","
    SEMICOLON = ";"

    LPAREN = "("
    RPAREN = ")"
    LBRACE = "{"
    RBRACE = "}"

    // Keywords
    FUNCTION = "FUNCTION"
    LET      = "LET"
*/

#[derive(PartialEq, Debug, Clone, Hash, Eq)]
pub enum TokenKind {
    Illegal,
    EOF,
    Identifier,
    Int,
    String,
    Assign,
    Plus,
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Function,
    Let,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Lt,
    Gt,
    Lte,
    Gte,

    True,
    False,
    If,
    Else,
    Return,

    Equal,
    NotEqual,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub raw: String,
}

impl Token {
    pub fn new(typ: TokenKind, literal: &str) -> Self {
        Self {
            kind: typ,
            raw: literal.to_string(),
        }
    }
}
