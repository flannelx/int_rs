use std::collections::VecDeque;

use crate::{
    lexer::Lexer,
    token::{Token, TokenKind},
};
use anyhow::bail;
use ast::{Expr, Infix, Literal, Prefix, Program, Stmt};

macro_rules! prec {
    ($token:expr) => {
        Precedence::from_token($token)
    };
}

macro_rules! bool {
    ($bool:expr) => {
        match $bool {
            "true" => true,
            "false" => false,
            _ => bail!("Expected a boolean string 'true' or 'false'"),
        }
    };
}

pub mod ast;

#[derive(PartialEq, Debug, PartialOrd)]
pub enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // myFunction(X)
    Index,
}

impl Precedence {
    fn from_token(kind: &TokenKind) -> Precedence {
        use Precedence::*;
        use TokenKind::*;
        #[cfg_attr(rustfmt, rustfmt_skip)]
        match kind {
            LParen              => Call,
            LBracket            => Index,
            Plus     | Minus    => Sum,
            Asterisk | Slash    => Product,
            Equal    | NotEqual => Equals,
            Lt | Gt | Lte | Gte => LessGreater,
            _                   => Lowest,
        }
    }
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    curr_token: Token,
    next_token: Token,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Lexer::new(input);
        let curr_token = lexer.next_token();
        let next_token = lexer.next_token();
        Self {
            lexer,
            curr_token,
            next_token,
        }
    }

    // This is just sugar for self.lexer next() && peek()
    //
    // Only use advance on parse_x_statment() or recursive parsing fn() to avoid confusion - ';'
    // Because we skip semicolon on 'curr', under parse_statement() you will have to manually skip
    // it or a recursive parsing fn() will skip it at it's base case
    //
    // Otherwise it is very easy to over advance and skip 'let' or under advance and gives ';' to a
    // prefix parse fn().
    //
    // Since parsing relys on ';' to stop, you can not auto skip ';' here, otherwise will over advance
    //
    // !TODO Looking for a better solution. Perhaps just lex all tokens and store in a Deque, but
    // that does not solve all the problem, we still have to check 'curr' token kind.
    //
    // Nom crate?
    pub fn advance(&mut self) {
        std::mem::swap(&mut self.curr_token, &mut self.next_token);
        self.next_token = self.lexer.next_token();
        // println!("{:?} {:?}", self.curr_token, self.next_token);
    }

    pub fn parse_program(&mut self) -> anyhow::Result<Program> {
        let mut program: Program = VecDeque::new();

        while self.curr_token.kind != TokenKind::EOF {
            program.push_back(self.parse_statement()?);
            if self.curr_token.kind == TokenKind::Semicolon {
                self.advance();
            }
        }

        Ok(program)
    }

    pub fn parse_statement(&mut self) -> anyhow::Result<Stmt> {
        match &self.curr_token.kind {
            TokenKind::Let => self.parse_let_statement(),
            TokenKind::Return => self.parse_return_statement(),
            _ => self.parse_expr_statement(),
        }
    }

    pub fn parse_expr(&mut self, prec: Precedence) -> anyhow::Result<Expr> {
        let left = self.parse_atom_expr()?;
        Ok(self.parse_expr_recursive(prec, left)?)
    }

    pub fn parse_expr_recursive(&mut self, prec: Precedence, left: Expr) -> anyhow::Result<Expr> {
        if matches!(self.curr_token.kind, TokenKind::EOF | TokenKind::Semicolon) {
            return Ok(left);
        }
        self.advance();
        match prec!(&self.curr_token.kind) {
            Precedence::Call if prec < Precedence::Call => {
                let expr = self.parse_call_expr(left)?;
                self.parse_expr_recursive(prec, expr)
            }
            Precedence::Index => todo!(),
            p if prec < p => {
                let left2 = self.parse_infix_expr(Box::new(left))?;
                self.parse_expr_recursive(prec, left2)
            }
            _ => Ok(left),
        }
    }

    pub fn parse_expr_statement(&mut self) -> anyhow::Result<Stmt> {
        let stmt = Stmt::ExprStmt(self.parse_expr(Precedence::Lowest)?);
        Ok(stmt)
    }

    pub fn parse_let_statement(&mut self) -> anyhow::Result<Stmt> {
        // Prev = let, Curr = ident, Next = '='
        self.advance();
        let ident = {
            if self.curr_token.kind != TokenKind::Identifier {
                bail!("Expected an identifier, got {:?}", self.next_token);
            }
            self.curr_token.raw.clone()
        };

        // Advance, Curr = '=', Next = expr
        self.advance();

        if self.curr_token.kind != TokenKind::Assign {
            bail!("Expeceted assign, got {:?}", self.next_token);
        }

        self.advance();
        let expr = self.parse_expr(Precedence::Lowest)?;
        Ok(Stmt::LetStatement { ident, expr })
    }

    pub fn parse_return_statement(&mut self) -> anyhow::Result<Stmt> {
        self.advance();
        Ok(Stmt::ReturnStatement(self.parse_expr(Precedence::Lowest)?))
    }

    pub fn parse_atom_expr(&mut self) -> anyhow::Result<Expr> {
        use TokenKind::*;
        #[cfg_attr(rustfmt, rustfmt_skip)]
        match &self.curr_token.kind {
            Plus | Minus | Bang => self.parse_prefix_expr(),
            True | False        => Ok(Expr::Literal(Literal::Bool(bool!(self.curr_token.raw.as_str())))),
            Identifier          => Ok(Expr::Identifier(self.curr_token.raw.clone())),
            Int                 => Ok(Expr::Literal(Literal::Int(self.curr_token.raw.parse::<i64>()?))),
            String              => Ok(Expr::Literal(Literal::String(self.curr_token.raw.clone()))),
            Function            => self.parse_fn_expr(),
            If                  => self.parse_if_expr(),
            t                   => bail!("Atom Expr: Unexpected token:{:?}", self.curr_token),
        }
    }

    pub fn parse_prefix_expr(&mut self) -> anyhow::Result<Expr> {
        let op = match &self.curr_token.kind {
            TokenKind::Plus => Prefix::Plus,
            TokenKind::Minus => Prefix::Minus,
            TokenKind::Bang => Prefix::Not,
            _ => bail!("Expected Prefix operator, got {:?}", self.curr_token),
        };
        self.advance();
        Ok(Expr::Prefix {
            op,
            right: Box::new(self.parse_atom_expr()?),
        })
    }

    pub fn parse_infix_expr(&mut self, left: Box<Expr>) -> anyhow::Result<Expr> {
        use TokenKind::*;
        #[cfg_attr(rustfmt, rustfmt_skip)]
        let op = match self.curr_token.kind {
            Plus     => Infix::Plus,
            Minus    => Infix::Minus,
            Slash    => Infix::Divide,
            Asterisk => Infix::Multiply,
            Equal    => Infix::Equal,
            NotEqual => Infix::NotEqual,
            Gt       => Infix::Gt,
            Lt       => Infix::Lt,
            Gte      => Infix::Gte,
            Lte      => Infix::Lte,
            _ => bail!("Expected a prefix operator, got {:?}", self.curr_token),
        };
        self.advance();
        if matches!(self.curr_token.kind, Semicolon | EOF) {
            bail!("Expected an expression");
        }
        let prec = prec!(&self.curr_token.kind);
        let right = self.parse_expr(prec)?;
        Ok(Expr::Infix {
            left,
            op,
            right: Box::new(right),
        })
    }

    pub fn parse_fn_expr(&mut self) -> anyhow::Result<Expr> {
        use TokenKind::*;
        // curr = fn, next = ident
        if self.curr_token.kind != Function {
            bail!("Expected a function token, got {:?}", self.curr_token);
        }

        // curr = ident, next = (
        self.advance();
        let ident = {
            if self.curr_token.kind != Identifier {
                bail!("Expected a function name");
            }
            self.curr_token.raw.to_string()
        };

        // curr = (, next = ..
        self.advance();

        // curr = .., next = ..
        self.advance();
        let mut params = Vec::new();
        // (, curr = ..
        while !matches!(self.curr_token.kind, RParen | EOF) {
            if !matches!(self.curr_token.kind, Comma | Identifier) {
                bail!(
                    "Unsupported syntax for function params {:?}",
                    self.curr_token
                );
            }
            if self.curr_token.kind == Identifier {
                params.push(self.curr_token.raw.to_string());
            }
            self.advance();
        }
        if self.curr_token.kind != RParen {
            bail!("Expected right parenthese, got {:?}", self.curr_token);
        }
        self.advance();
        let body = self.parse_block_stmt()?;
        Ok(Expr::Function {
            ident,
            params,
            body,
        })
    }

    pub fn parse_if_expr(&mut self) -> anyhow::Result<Expr> {
        use TokenKind::*;
        if self.curr_token.kind != If {
            bail!("Expected 'if', got {:?}", self.curr_token);
        }
        self.advance();
        if self.curr_token.kind != LParen {
            bail!("Expected '(' for condition, got {:?}", self.curr_token);
        }
        self.advance();
        let cond = self.parse_expr(Precedence::Lowest)?;
        if self.curr_token.kind == RParen {
            self.advance();
        }
        let program = self.parse_block_stmt()?;
        if matches!(self.curr_token.kind, RBrace) {
            self.advance()
        }
        let alt = {
            if self.curr_token.kind != Else {
                None
            } else {
                self.advance();
                Some(self.parse_block_stmt()?)
            }
        };
        if matches!(self.curr_token.kind, RBrace) {
            self.advance()
        }
        Ok(Expr::If {
            cond: Box::new(cond),
            body: program,
            alt,
        })
    }

    pub fn parse_call_expr(&mut self, ident: Expr) -> anyhow::Result<Expr> {
        use TokenKind::*;
        let func = {
            if let Expr::Identifier(func) = ident {
                func
            } else {
                bail!("Expected call identifier, got {:?}", self.curr_token);
            }
        };
        if self.curr_token.kind != LParen {
            bail!("Expected left parenthese, got {:?}", self.curr_token);
        }
        self.advance();
        let mut args = Vec::new();
        while !matches!(self.curr_token.kind, Semicolon | RParen | EOF) {
            if self.curr_token.kind != Comma {
                args.push(self.parse_expr(Precedence::Lowest)?);
            }
            self.advance();
        }
        if matches!(self.curr_token.kind, RParen | Semicolon) {
            self.advance();
        }
        Ok(Expr::Call { func, args })
    }

    pub fn parse_block_stmt(&mut self) -> anyhow::Result<Program> {
        use TokenKind::*;
        if self.curr_token.kind != LBrace {
            bail!("Expected a '{{', got {:?}", self.curr_token);
        }
        self.advance();
        let mut program: Program = VecDeque::new();
        while !matches!(self.curr_token.kind, EOF | RBrace) {
            program.push_back(self.parse_statement()?);
            self.advance();
        }
        if self.curr_token.kind == RBrace {
            self.advance();
        }
        Ok(program)
    }
}

#[test]
fn parse_prefix_test() {
    let input = r#"
    x 123 "123"
    "#;
    let mut p = Parser::new(input);
    let mut v = Vec::new();
    while p.curr_token.kind != TokenKind::EOF {
        v.push(p.parse_atom_expr().unwrap());
        p.advance();
    }
    let expected = vec![
        Expr::Identifier("x".to_string()),
        Expr::Literal(Literal::Int(123)),
        Expr::Literal(Literal::String("123".to_string())),
    ];
    assert!(expected == v);
}

#[test]
fn parse_stmt() {
    let input = r#"
    let x = 100;
    let y = 200;
    let s = "123";
    let b = true;
    let b = false;

    x + y;
    x - y;
    x * y;

    let z = x + y;
    let z = x - y;
    let z = x * y;
    let z = x + 300;
    let z = x - 300;
    let z = x * 300;

    x < y;
    x > y;
    x == y;
    x <= y;
    x >= y;

    let eq = x == y;
    let lt = x < y;
    let gt = x > y;
    let lte = x <= y;
    let gte = x >= y;

    return x;
    return 123;
    return x + y;
    return x * 1;
    return 1 + 2;
    return 1 * 2;


    !false;
    !true;
    +200;
    -100;
    "#;
    let mut p = Parser::new(input);
    let program = p.parse_program().unwrap();
    #[cfg_attr(rustfmt, rustfmt_skip)]
    let expected = vec![
       Stmt::LetStatement { ident: "x".to_string(), expr: Expr::Literal(Literal::Int(100)) },
       Stmt::LetStatement { ident: "y".to_string(), expr: Expr::Literal(Literal::Int(200)) },
       Stmt::LetStatement { ident: "s".to_string(), expr: Expr::Literal(Literal::String("123".to_string())) },
       Stmt::LetStatement { ident: "b".to_string(), expr: Expr::Literal(Literal::Bool(true))},
       Stmt::LetStatement { ident: "b".to_string(), expr: Expr::Literal(Literal::Bool(false))},

       Stmt::ExprStmt(Expr::Infix { left: Box::new(Expr::Identifier("x".to_string())), op: Infix::Plus, right: Box::new(Expr::Identifier("y".to_string())) }),
       Stmt::ExprStmt(Expr::Infix { left: Box::new(Expr::Identifier("x".to_string())), op: Infix::Minus, right: Box::new(Expr::Identifier("y".to_string())) }),
       Stmt::ExprStmt(Expr::Infix { left: Box::new(Expr::Identifier("x".to_string())), op: Infix::Multiply, right: Box::new(Expr::Identifier("y".to_string())) }),

       Stmt::LetStatement { ident: "z".to_string(), expr: Expr::Infix { left: Box::new(Expr::Identifier("x".to_string())), op: Infix::Plus, right: Box::new(Expr::Identifier("y".to_string())) } },
       Stmt::LetStatement { ident: "z".to_string(), expr: Expr::Infix { left: Box::new(Expr::Identifier("x".to_string())), op: Infix::Minus, right: Box::new(Expr::Identifier("y".to_string())) } },
       Stmt::LetStatement { ident: "z".to_string(), expr: Expr::Infix { left: Box::new(Expr::Identifier("x".to_string())), op: Infix::Multiply, right: Box::new(Expr::Identifier("y".to_string())) } },
       Stmt::LetStatement { ident: "z".to_string(), expr: Expr::Infix { left: Box::new(Expr::Identifier("x".to_string())), op: Infix::Plus, right: Box::new(Expr::Literal(Literal::Int(300))) } },
       Stmt::LetStatement { ident: "z".to_string(), expr: Expr::Infix { left: Box::new(Expr::Identifier("x".to_string())), op: Infix::Minus, right: Box::new(Expr::Literal(Literal::Int(300))) } },
       Stmt::LetStatement { ident: "z".to_string(), expr: Expr::Infix { left: Box::new(Expr::Identifier("x".to_string())), op: Infix::Multiply, right: Box::new(Expr::Literal(Literal::Int(300))) } },

       Stmt::ExprStmt(Expr::Infix { left: Box::new(Expr::Identifier("x".to_string())), op: Infix::Lt, right: Box::new(Expr::Identifier("y".to_string())) }),
       Stmt::ExprStmt(Expr::Infix { left: Box::new(Expr::Identifier("x".to_string())), op: Infix::Gt, right: Box::new(Expr::Identifier("y".to_string())) }),
       Stmt::ExprStmt(Expr::Infix { left: Box::new(Expr::Identifier("x".to_string())), op: Infix::Equal, right: Box::new(Expr::Identifier("y".to_string())) }),
       Stmt::ExprStmt(Expr::Infix { left: Box::new(Expr::Identifier("x".to_string())), op: Infix::Lte, right: Box::new(Expr::Identifier("y".to_string())) }),
       Stmt::ExprStmt(Expr::Infix { left: Box::new(Expr::Identifier("x".to_string())), op: Infix::Gte, right: Box::new(Expr::Identifier("y".to_string())) }),


       Stmt::LetStatement { ident: "eq".to_string(), expr: Expr::Infix { left: Box::new(Expr::Identifier("x".to_string())), op: Infix::Equal, right: Box::new(Expr::Identifier("y".to_string())) } },
       Stmt::LetStatement { ident: "lt".to_string(), expr: Expr::Infix { left: Box::new(Expr::Identifier("x".to_string())), op: Infix::Lt, right: Box::new(Expr::Identifier("y".to_string())) } },
       Stmt::LetStatement { ident: "gt".to_string(), expr: Expr::Infix { left: Box::new(Expr::Identifier("x".to_string())), op: Infix::Gt, right: Box::new(Expr::Identifier("y".to_string())) } },
       Stmt::LetStatement { ident: "lte".to_string(), expr: Expr::Infix { left: Box::new(Expr::Identifier("x".to_string())), op: Infix::Lte, right: Box::new(Expr::Identifier("y".to_string())) } },
       Stmt::LetStatement { ident: "gte".to_string(), expr: Expr::Infix { left: Box::new(Expr::Identifier("x".to_string())), op: Infix::Gte, right: Box::new(Expr::Identifier("y".to_string())) } },

       Stmt::ReturnStatement(Expr::Identifier("x".to_string())),
       Stmt::ReturnStatement(Expr::Literal(Literal::Int(123))),
       Stmt::ReturnStatement(Expr::Infix { left: Box::new(Expr::Identifier("x".to_string())), op: Infix::Plus, right: Box::new(Expr::Identifier("y".to_string()))}),
       Stmt::ReturnStatement(Expr::Infix { left: Box::new(Expr::Identifier("x".to_string())), op: Infix::Multiply, right: Box::new(Expr::Literal(Literal::Int(1)))}),
       Stmt::ReturnStatement(Expr::Infix { left: Box::new(Expr::Literal(Literal::Int(1))), op: Infix::Plus, right: Box::new(Expr::Literal(Literal::Int(2)))}),
       Stmt::ReturnStatement(Expr::Infix { left: Box::new(Expr::Literal(Literal::Int(1))), op: Infix::Multiply, right: Box::new(Expr::Literal(Literal::Int(2)))}),

       Stmt::ExprStmt(Expr::Prefix { op: Prefix::Not, right: Box::new(Expr::Literal(Literal::Bool(false))) }),
       Stmt::ExprStmt(Expr::Prefix { op: Prefix::Not, right: Box::new(Expr::Literal(Literal::Bool(true))) }),
       Stmt::ExprStmt(Expr::Prefix { op: Prefix::Plus, right: Box::new(Expr::Literal(Literal::Int(200))) }),
       Stmt::ExprStmt(Expr::Prefix { op: Prefix::Minus, right: Box::new(Expr::Literal(Literal::Int(100))) }),
    ];

    for (res, exp) in program.iter().zip(expected.iter()) {
        assert_eq!(res, exp);
    }
}

#[test]
fn parse_fn_test() {
    let input = r#"
    fn sum(x, y) {
        return x + y;
    };
    fn sum(x, y) {
        x + y
    };
    "#;
    let mut p = Parser::new(input);
    let program = p.parse_program().unwrap();
    let expected = vec![
        Stmt::ExprStmt(Expr::Function {
            ident: "sum".to_string(),
            params: vec!["x".to_string(), "y".to_string()],
            body: vec![Stmt::ReturnStatement(Expr::Infix {
                left: Box::new(Expr::Identifier("x".to_string())),
                op: Infix::Plus,
                right: Box::new(Expr::Identifier("y".to_string())),
            })]
            .into(),
        }),
        Stmt::ExprStmt(Expr::Function {
            ident: "sum".to_string(),
            params: vec!["x".to_string(), "y".to_string()],
            body: vec![Stmt::ExprStmt(Expr::Infix {
                left: Box::new(Expr::Identifier("x".to_string())),
                op: Infix::Plus,
                right: Box::new(Expr::Identifier("y".to_string())),
            })]
            .into(),
        }),
    ];

    for (p, e) in program.iter().zip(expected.iter()) {
        assert_eq!(p, e)
    }
}

#[test]
fn parse_call_test() {
    let input = r#"
    fn sum(x, y) {
        x + y;
    };
    let s = sum(100, 10);
    "#;
    let mut p = Parser::new(input);
    let program = p.parse_program().unwrap();
    let expected = vec![
        Stmt::ExprStmt(Expr::Function {
            ident: "sum".to_string(),
            params: vec!["x".to_string(), "y".to_string()],
            body: vec![Stmt::ExprStmt(Expr::Infix {
                left: Box::new(Expr::Identifier("x".to_string())),
                op: Infix::Plus,
                right: Box::new(Expr::Identifier("y".to_string())),
            })]
            .into(),
        }),
        Stmt::LetStatement {
            ident: "s".to_string(),
            expr: Expr::Call {
                func: "sum".to_string(),
                args: vec![
                    Expr::Literal(Literal::Int(100)),
                    Expr::Literal(Literal::Int(10)),
                ],
            },
        },
    ];

    for (p, e) in program.iter().zip(expected.iter()) {
        assert_eq!(p, e)
    }
}

#[test]
fn parse_if_test() {
    let input = r#"
    let x = 100;
    if (2 == 2) {
        x + 1;
    };
    let x = 100;
    if (2 == 2) {
        x + 1;
    } else {
        1 + 1;
    };
    let x = 100;
    "#;
    let mut p = Parser::new(input);
    let program = p.parse_program().unwrap();
    let expected = vec![
        Stmt::LetStatement {
            ident: "x".to_string(),
            expr: Expr::Literal(Literal::Int(100)),
        },
        Stmt::ExprStmt(Expr::If {
            cond: Box::new(Expr::Infix {
                left: Box::new(Expr::Literal(Literal::Int(2))),
                op: Infix::Equal,
                right: Box::new(Expr::Literal(Literal::Int(2))),
            }),
            body: vec![Stmt::ExprStmt(Expr::Infix {
                left: Box::new(Expr::Identifier("x".to_string())),
                op: Infix::Plus,
                right: Box::new(Expr::Literal(Literal::Int(1))),
            })]
            .into(),
            alt: None,
        }),
        Stmt::LetStatement {
            ident: "x".to_string(),
            expr: Expr::Literal(Literal::Int(100)),
        },
        Stmt::ExprStmt(Expr::If {
            cond: Box::new(Expr::Infix {
                left: Box::new(Expr::Literal(Literal::Int(2))),
                op: Infix::Equal,
                right: Box::new(Expr::Literal(Literal::Int(2))),
            }),
            body: vec![Stmt::ExprStmt(Expr::Infix {
                left: Box::new(Expr::Identifier("x".to_string())),
                op: Infix::Plus,
                right: Box::new(Expr::Literal(Literal::Int(1))),
            })]
            .into(),
            alt: Some(
                vec![Stmt::ExprStmt(Expr::Infix {
                    left: Box::new(Expr::Literal(Literal::Int(1))),
                    op: Infix::Plus,
                    right: Box::new(Expr::Literal(Literal::Int(1))),
                })]
                .into(),
            ),
        }),
        Stmt::LetStatement {
            ident: "x".to_string(),
            expr: Expr::Literal(Literal::Int(100)),
        },
    ];

    for (p, e) in program.iter().zip(expected.iter()) {
        assert_eq!(p, e)
    }
}
