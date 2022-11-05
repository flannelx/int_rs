#![allow(dead_code)]

use crate::token::{Token, TokenType};
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
        while let Some(c) = self.input.peek() && (c == &' ' || c == &'\t' || c == &'\n' || c == &'\r') {
            self.input.next();
        }
    }

    pub fn next_ident(&mut self) -> Option<Token> {
        let mut ident_literal = String::new();
        while let Some(c) = self.input.peek() && Self::char_to_token(c) == TokenType::IDENT {
            ident_literal.push(self.input.next().unwrap());
        }
        if ident_literal.is_empty() {
            return None;
        }
        if let Some(keyword) = Self::check_keyword(&ident_literal) {
            return Some(Token::new(keyword, &ident_literal));
        }
        return Some(Token::new(TokenType::IDENT, &ident_literal));
    }

    pub fn next_int(&mut self) -> Option<Token> {
        let mut int_literal = String::new();
        while let Some(c) = self.input.peek() && Self::char_to_token(c) == TokenType::INT {
            int_literal.push(self.input.next().unwrap());
        }

        if int_literal.is_empty() {
            return None;
        }
        return Some(Token::new(TokenType::INT, &int_literal));
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        if self.input.peek().is_none() {
            return Token::new(TokenType::EOF, "");
        }
        if let Some(token) = self.next_ident() {
            return token;
        }
        if let Some(token) = self.next_int() {
            return token;
        }

        // Check '==' '!=' etc, should be in its own function later on
        let c = self.input.next().unwrap();
        if (c == '=' || c == '!') && let Some(nxt_c) = self.input.peek() {
            let literal: String = vec![c, *nxt_c].iter().collect();
            if let Some(keyword) = Self::check_keyword(&literal) {
                self.input.next();
                return Token::new(keyword, &literal);
            }
        }
        return Token::new(Self::char_to_token(&c), &c.to_string());
    }

    pub fn char_to_token(c: &char) -> TokenType {
        match c {
            '=' => TokenType::ASSIGN,
            '+' => TokenType::PLUS,
            '(' => TokenType::LPAREN,
            ')' => TokenType::RPAREN,
            '{' => TokenType::LBRACE,
            '}' => TokenType::RBRACE,
            ',' => TokenType::COMMA,
            ';' => TokenType::SEMICOLON,
            '-' => TokenType::MINUS,
            '!' => TokenType::BANG,
            '*' => TokenType::ASTERISK,
            '/' => TokenType::SLASH,
            '<' => TokenType::LT,
            '>' => TokenType::GT,
            '0'..='9' => TokenType::INT,
            '_' | 'A'..='z' => TokenType::IDENT,
            _ => TokenType::ILLEGAL,
            //_ => panic!("not yet implemented"),
        }
    }

    pub fn check_keyword(s: &str) -> Option<TokenType> {
        match s {
            "let" => Some(TokenType::LET),
            "fn" => Some(TokenType::FUNCTION),
            "true" => Some(TokenType::TRUE),
            "false" => Some(TokenType::FALSE),
            "if" => Some(TokenType::IF),
            "else" => Some(TokenType::ELSE),
            "return" => Some(TokenType::RETURN),
            "!=" => Some(TokenType::NotEQ),
            "==" => Some(TokenType::EQ),
            _ => None,
            //_ => TokenType::ILLEGAL,
        }
    }
}

#[test]
fn test_next_token() {
    let mut lexer = Lexer::new(
        r#"
        let five = 5;
        let ten = 10;

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
    "#,
    );
    let tests = vec![
        Token::new(TokenType::LET, "let"),
        Token::new(TokenType::IDENT, "five"),
        Token::new(TokenType::ASSIGN, "="),
        Token::new(TokenType::INT, "5"),
        Token::new(TokenType::SEMICOLON, ";"),
        Token::new(TokenType::LET, "let"),
        Token::new(TokenType::IDENT, "ten"),
        Token::new(TokenType::ASSIGN, "="),
        Token::new(TokenType::INT, "10"),
        Token::new(TokenType::SEMICOLON, ";"),
        Token::new(TokenType::LET, "let"),
        Token::new(TokenType::IDENT, "add"),
        Token::new(TokenType::ASSIGN, "="),
        Token::new(TokenType::FUNCTION, "fn"),
        Token::new(TokenType::LPAREN, "("),
        Token::new(TokenType::IDENT, "x"),
        Token::new(TokenType::COMMA, ","),
        Token::new(TokenType::IDENT, "y"),
        Token::new(TokenType::RPAREN, ")"),
        Token::new(TokenType::LBRACE, "{"),
        Token::new(TokenType::IDENT, "x"),
        Token::new(TokenType::PLUS, "+"),
        Token::new(TokenType::IDENT, "y"),
        Token::new(TokenType::SEMICOLON, ";"),
        Token::new(TokenType::RBRACE, "}"),
        Token::new(TokenType::SEMICOLON, ";"),
        Token::new(TokenType::LET, "let"),
        Token::new(TokenType::IDENT, "result"),
        Token::new(TokenType::ASSIGN, "="),
        Token::new(TokenType::IDENT, "add"),
        Token::new(TokenType::LPAREN, "("),
        Token::new(TokenType::IDENT, "five"),
        Token::new(TokenType::COMMA, ","),
        Token::new(TokenType::IDENT, "ten"),
        Token::new(TokenType::RPAREN, ")"),
        Token::new(TokenType::SEMICOLON, ";"),
        Token::new(TokenType::BANG, "!"),
        Token::new(TokenType::MINUS, "-"),
        Token::new(TokenType::SLASH, "/"),
        Token::new(TokenType::ASTERISK, "*"),
        Token::new(TokenType::INT, "5"),
        Token::new(TokenType::SEMICOLON, ";"),
        Token::new(TokenType::INT, "5"),
        Token::new(TokenType::LT, "<"),
        Token::new(TokenType::INT, "10"),
        Token::new(TokenType::GT, ">"),
        Token::new(TokenType::INT, "5"),
        Token::new(TokenType::SEMICOLON, ";"),
        Token::new(TokenType::IF, "if"),
        Token::new(TokenType::LPAREN, "("),
        Token::new(TokenType::INT, "5"),
        Token::new(TokenType::LT, "<"),
        Token::new(TokenType::INT, "10"),
        Token::new(TokenType::RPAREN, ")"),
        Token::new(TokenType::LBRACE, "{"),
        Token::new(TokenType::RETURN, "return"),
        Token::new(TokenType::TRUE, "true"),
        Token::new(TokenType::SEMICOLON, ";"),
        Token::new(TokenType::RBRACE, "}"),
        Token::new(TokenType::ELSE, "else"),
        Token::new(TokenType::LBRACE, "{"),
        Token::new(TokenType::RETURN, "return"),
        Token::new(TokenType::FALSE, "false"),
        Token::new(TokenType::SEMICOLON, ";"),
        Token::new(TokenType::RBRACE, "}"),
        Token::new(TokenType::INT, "10"),
        Token::new(TokenType::EQ, "=="),
        Token::new(TokenType::INT, "10"),
        Token::new(TokenType::SEMICOLON, ";"),
        Token::new(TokenType::INT, "10"),
        Token::new(TokenType::NotEQ, "!="),
        Token::new(TokenType::INT, "9"),
        Token::new(TokenType::SEMICOLON, ";"),
    ];
    for expected in tests {
        let token = lexer.next_token();
        println!("\n{:?}\n{:?}", expected, token);
        assert!(token == expected);
    }
}
