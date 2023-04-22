use crate::token::{Token, TokenKind};
use std::{iter::Peekable, str::Chars};

pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer<'a> {
        Self {
            input: input.chars().peekable(),
        }
    }

    pub fn skip_whitespace(&mut self) {
        while let Some(c) = self.input.peek() && c.is_whitespace() {
            self.input.next();
        }
    }

    pub fn next_ident(&mut self) -> Option<Token> {
        let mut ident_literal = String::new();
        while let Some(c) = self.input.peek() && Self::char_to_token(c) == TokenKind::Identifier {
            ident_literal.push(self.input.next().unwrap());
        }
        if ident_literal.is_empty() {
            return None;
        }
        if let Some(keyword) = Self::check_keyword(&ident_literal) {
            return Some(Token::new(keyword, &ident_literal));
        }

        Some(Token::new(TokenKind::Identifier, &ident_literal))
    }

    pub fn next_int(&mut self) -> Option<Token> {
        let mut int_literal = String::new();
        while let Some(c) = self.input.peek() && Self::char_to_token(c) == TokenKind::Int {
            int_literal.push(self.input.next().unwrap());
        }

        if int_literal.is_empty() {
            return None;
        }

        Some(Token::new(TokenKind::Int, &int_literal))
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        if self.input.peek().is_none() {
            return Token::new(TokenKind::EOF, "");
        }
        if let Some(token) = self.next_ident() {
            return token;
        }
        if let Some(token) = self.next_int() {
            return token;
        }
        if let Some(token) = self.parse_string() {
            return token;
        }

        // Check '==' '!=' etc, should be in its own function later on
        let c = self.input.next().unwrap();
        if matches!(c, '=' | '!' | '<' | '>') {
            if let Some(nxt_c) = self.input.peek() {
                let literal: String = vec![c, *nxt_c].iter().collect();
                if let Some(keyword) = Self::check_keyword(&literal) {
                    self.input.next();
                    return Token::new(keyword, &literal);
                }
            }
        }

        Token::new(Self::char_to_token(&c), &c.to_string())
    }

    pub fn parse_string(&mut self) -> Option<Token> {
        if self.input.peek().is_none() || self.input.peek().unwrap() != &'"' {
            return None;
        }
        self.input.next();
        let mut s = String::new();
        while let Some(c) = self.input.next() && c != '"'{
            s.push(c);
        }
        Some(Token {
            kind: TokenKind::String,
            raw: s,
        })
    }

    pub fn char_to_token(c: &char) -> TokenKind {
        match c {
            '=' => TokenKind::Assign,
            '+' => TokenKind::Plus,
            '(' => TokenKind::LParen,
            ')' => TokenKind::RParen,
            '{' => TokenKind::LBrace,
            '}' => TokenKind::RBrace,
            '[' => TokenKind::LBracket,
            ']' => TokenKind::RBracket,
            ',' => TokenKind::Comma,
            ';' => TokenKind::Semicolon,
            '-' => TokenKind::Minus,
            '!' => TokenKind::Bang,
            '*' => TokenKind::Asterisk,
            '/' => TokenKind::Slash,
            '<' => TokenKind::Lt,
            '>' => TokenKind::Gt,
            '0'..='9' => TokenKind::Int,
            '_' | 'A'..='z' => TokenKind::Identifier,
            _ => TokenKind::Illegal,
            //_ => panic!("not yet implemented"),
        }
    }

    pub fn check_keyword(s: &str) -> Option<TokenKind> {
        let kind = match s {
            "let" => TokenKind::Let,
            "fn" => TokenKind::Function,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "return" => TokenKind::Return,
            "!=" => TokenKind::NotEqual,
            "==" => TokenKind::Equal,
            "<=" => TokenKind::Lte,
            ">=" => TokenKind::Gte,
            _ => return None,
            //_ => TokenType::ILLEGAL,
        };
        Some(kind)
    }
}

#[test]
fn test_next_token() {
    let mut lexer = Lexer::new(
        r#"
        let five = 5;
        let ten = 10;
        let s = "123";

        let add = fn(x, y) {
          x + y;
        };

        let result = add(five, ten);
        !-/*5;
        5 < 10 > 5;

        if (5 < 10) {
            return true;
        } else {
            return false;
        }

        10 == 10;
        10 != 9;
        10 <= 9;
        10 >= 9;
    "#,
    );
    let tests = vec![
        Token::new(TokenKind::Let, "let"),
        Token::new(TokenKind::Identifier, "five"),
        Token::new(TokenKind::Assign, "="),
        Token::new(TokenKind::Int, "5"),
        Token::new(TokenKind::Semicolon, ";"),
        Token::new(TokenKind::Let, "let"),
        Token::new(TokenKind::Identifier, "ten"),
        Token::new(TokenKind::Assign, "="),
        Token::new(TokenKind::Int, "10"),
        Token::new(TokenKind::Semicolon, ";"),
        Token::new(TokenKind::Let, "let"),
        Token::new(TokenKind::Identifier, "s"),
        Token::new(TokenKind::Assign, "="),
        Token::new(TokenKind::String, "123"),
        Token::new(TokenKind::Semicolon, ";"),
        Token::new(TokenKind::Let, "let"),
        Token::new(TokenKind::Identifier, "add"),
        Token::new(TokenKind::Assign, "="),
        Token::new(TokenKind::Function, "fn"),
        Token::new(TokenKind::LParen, "("),
        Token::new(TokenKind::Identifier, "x"),
        Token::new(TokenKind::Comma, ","),
        Token::new(TokenKind::Identifier, "y"),
        Token::new(TokenKind::RParen, ")"),
        Token::new(TokenKind::LBrace, "{"),
        Token::new(TokenKind::Identifier, "x"),
        Token::new(TokenKind::Plus, "+"),
        Token::new(TokenKind::Identifier, "y"),
        Token::new(TokenKind::Semicolon, ";"),
        Token::new(TokenKind::RBrace, "}"),
        Token::new(TokenKind::Semicolon, ";"),
        Token::new(TokenKind::Let, "let"),
        Token::new(TokenKind::Identifier, "result"),
        Token::new(TokenKind::Assign, "="),
        Token::new(TokenKind::Identifier, "add"),
        Token::new(TokenKind::LParen, "("),
        Token::new(TokenKind::Identifier, "five"),
        Token::new(TokenKind::Comma, ","),
        Token::new(TokenKind::Identifier, "ten"),
        Token::new(TokenKind::RParen, ")"),
        Token::new(TokenKind::Semicolon, ";"),
        Token::new(TokenKind::Bang, "!"),
        Token::new(TokenKind::Minus, "-"),
        Token::new(TokenKind::Slash, "/"),
        Token::new(TokenKind::Asterisk, "*"),
        Token::new(TokenKind::Int, "5"),
        Token::new(TokenKind::Semicolon, ";"),
        Token::new(TokenKind::Int, "5"),
        Token::new(TokenKind::Lt, "<"),
        Token::new(TokenKind::Int, "10"),
        Token::new(TokenKind::Gt, ">"),
        Token::new(TokenKind::Int, "5"),
        Token::new(TokenKind::Semicolon, ";"),
        Token::new(TokenKind::If, "if"),
        Token::new(TokenKind::LParen, "("),
        Token::new(TokenKind::Int, "5"),
        Token::new(TokenKind::Lt, "<"),
        Token::new(TokenKind::Int, "10"),
        Token::new(TokenKind::RParen, ")"),
        Token::new(TokenKind::LBrace, "{"),
        Token::new(TokenKind::Return, "return"),
        Token::new(TokenKind::True, "true"),
        Token::new(TokenKind::Semicolon, ";"),
        Token::new(TokenKind::RBrace, "}"),
        Token::new(TokenKind::Else, "else"),
        Token::new(TokenKind::LBrace, "{"),
        Token::new(TokenKind::Return, "return"),
        Token::new(TokenKind::False, "false"),
        Token::new(TokenKind::Semicolon, ";"),
        Token::new(TokenKind::RBrace, "}"),
        Token::new(TokenKind::Int, "10"),
        Token::new(TokenKind::Equal, "=="),
        Token::new(TokenKind::Int, "10"),
        Token::new(TokenKind::Semicolon, ";"),
        Token::new(TokenKind::Int, "10"),
        Token::new(TokenKind::NotEqual, "!="),
        Token::new(TokenKind::Int, "9"),
        Token::new(TokenKind::Semicolon, ";"),
        Token::new(TokenKind::Int, "10"),
        Token::new(TokenKind::Lte, "<="),
        Token::new(TokenKind::Int, "9"),
        Token::new(TokenKind::Semicolon, ";"),
        Token::new(TokenKind::Int, "10"),
        Token::new(TokenKind::Gte, ">="),
        Token::new(TokenKind::Int, "9"),
        Token::new(TokenKind::Semicolon, ";"),
    ];
    for expected in tests {
        let token = lexer.next_token();
        // println!("\n{:?}\n{:?}", expected, token);
        assert!(token == expected);
    }
}
