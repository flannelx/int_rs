#![allow(dead_code)]

use std::collections::HashMap;
use anyhow::anyhow;
use crate::{
    ast::{Expression, Identifier, LetStatement, Program, ReturnStatement, Statement},
    lexer::Lexer,
    token::{Token, TokenType},
};

pub enum Precedence {
    IOTA,
    LOWEST,
    EQUALS,      // ==
    LESSGREATER, // > or <
    SUM,         // +
    PRODUCT,     // *
    PREFIX,      // -X or !X
    CALL,        // myFunction(X)
}

type PreParseFn = Box<dyn FnMut() -> Expression>;
type InParseFn = Box<dyn FnMut(Expression) -> Expression>;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    prefix_parse_fn: HashMap<TokenType, PreParseFn>,
    infix_parse_fn: HashMap<TokenType, InParseFn>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self {
            lexer,
            prefix_parse_fn: HashMap::new(),
            infix_parse_fn: HashMap::new()
        }
    }

    pub fn parse_program(&mut self) -> anyhow::Result<Program> {
        let mut program = Program::default();
        loop {
            let token = self.lexer.next_token();
            match token.typ {
                TokenType::LET => program
                    .statements
                    .push(self.parse_let_statement(token.clone())?),

                TokenType::RETURN => program
                    .statements
                    .push(self.parse_return_statement(token.clone())?),

                TokenType::EOF => break,
                _ => (),
            }
        }
        Ok(program)
    }

    pub fn parse_let_statement(&mut self, token: Token) -> anyhow::Result<Statement> {
        let identifier = self.parse_identifier()?;
        let sign = self.lexer.next_token();
        if sign.typ != TokenType::ASSIGN {
            return Err(anyhow!("Parse error: No equal sign"));
        }
        let value = self.lexer.next_token();
        let expression = Expression {
            value: value.literal.clone(),
            token: value,
        };

        Ok(Statement::LetStatement(LetStatement {
            token,
            identifier,
            expression,
        }))
    }

    pub fn parse_return_statement(&mut self, token: Token) -> anyhow::Result<Statement> {
        let expression_token = self.lexer.next_token();
        let expression = Expression {
            value: expression_token.literal.clone(),
            token: expression_token,
        };
        Ok(Statement::ReturnStatement(ReturnStatement {
            token,
            expression,
        }))
    }

    pub fn parse_identifier(&mut self) -> anyhow::Result<Identifier> {
        let token = self.lexer.next_token();
        if token.typ != TokenType::IDENT {
            return Err(anyhow!("Expected a identifier"));
        }
        Ok(Identifier {
            value: token.literal.clone(),
            token,
        })
    }

    pub fn parse_expression(
        &mut self,
        token: Token,
        prec: Precedence,
    ) -> anyhow::Result<Expression> {
        todo!()
    }

    pub fn register_prefix(&mut self, typ: TokenType, func: PreParseFn){
        self.prefix_parse_fn.insert(typ, func);
    }

    pub fn register_infix(&mut self, typ: TokenType, func: InParseFn){
        self.infix_parse_fn.insert(typ, func);
    }
}

#[test]
fn test_let_statement() {
    let input = r#"
        let x = 5;
        let y = 10;
        let foobar = 838383;
    "#;
    let mut parser = Parser::new(Lexer::new(input));
    let program = parser.parse_program().unwrap();
    assert!(format!("{}", program) == "let x = 5;let y = 10;let foobar = 838383;");
}

#[test]
fn test_return_statement() {
    let input = r#"
        return 5;
        return 10;
        return 993322;
    "#;
    let mut parser = Parser::new(Lexer::new(input));
    let program = parser.parse_program().unwrap();
    assert!(format!("{}", program) == "return 5;return 10;return 993322;");
}

#[test]
fn test_string() {
    let input = r#"
        let foobar = 838383;
    "#;
    let mut parser = Parser::new(Lexer::new(input));
    let program = parser.parse_program().unwrap();
    assert!(format!("{}", program) == "let foobar = 838383;")
}

#[test]
fn test_ident() {
    let input = r#"
        foobar;
    "#;
    let mut parser = Parser::new(Lexer::new(input));
}
