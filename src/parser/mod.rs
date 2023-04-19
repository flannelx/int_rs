use crate::{
    lexer::Lexer,
    token::{Token, TokenKind},
};
use anyhow::bail;
use ast::{Expr, Infix, Prefix, Program, Stmt};

macro_rules! prec {
    ($token:expr) => {
        Precedence::from_token($token)
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

    pub fn advance(&mut self) {
        std::mem::swap(&mut self.curr_token, &mut self.next_token);
        self.next_token = self.lexer.next_token();
        // println!("{:?} {:?}", self.curr_token, self.next_token);
    }

    pub fn parse_program(&mut self) -> anyhow::Result<Program> {
        let mut program = Program { stmt: vec![] };

        while self.curr_token.kind != TokenKind::EOF {
            program.stmt.push(self.parse_statement()?);
            self.advance();
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
        match prec!(&self.next_token.kind) {
            Precedence::Call => todo!(),
            Precedence::Index => todo!(),
            p if p < prec!(&self.curr_token.kind) => {
                let left2 = self.parse_infix_expr(Box::new(left))?;
                Ok(self.parse_expr_recursive(prec, left2)?)
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
        todo!()
    }

    pub fn parse_atom_expr(&mut self) -> anyhow::Result<Expr> {
        use TokenKind::*;
        match &self.curr_token.kind {
            Identifier => Ok(Expr::Identifier(self.curr_token.raw.clone())),
            Int => Ok(Expr::Integer(self.curr_token.raw.parse::<i64>()?)),
            String => Ok(Expr::String(self.curr_token.raw.clone())),
            Function => todo!(),
            Plus | Minus | Bang => Ok(self.parse_prefix_expr()?),
            True => todo!(),
            False => todo!(),
            If => todo!(),
            t => bail!("Expected a prefix, got {:?}", self.curr_token),
        }
    }

    pub fn parse_prefix_expr(&mut self) -> anyhow::Result<Expr> {
        let op = match &self.curr_token.kind {
            TokenKind::Plus => Prefix::Plus,
            TokenKind::Minus => Prefix::Minus,
            TokenKind::Bang => Prefix::Not,
            _ => bail!("Expected Prefix operator, got {:?}", self.curr_token),
        };
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
        Expr::Integer(123),
        Expr::String("123".to_string()),
    ];
    assert!(expected == v);
}

#[test]
fn parse_let_stmt() {
    let input = r#"
    let x = 100;
    let y = 200;
    let s = "123";

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
    "#;
    let mut p = Parser::new(input);
    let program = p.parse_program().unwrap();
    #[cfg_attr(rustfmt, rustfmt_skip)]
    let expected = vec![
       Stmt::LetStatement { ident: "x".to_string(), expr: Expr::Integer(100) },
       Stmt::LetStatement { ident: "y".to_string(), expr: Expr::Integer(200) },
       Stmt::LetStatement { ident: "s".to_string(), expr: Expr::String("123".to_string()) },

       Stmt::ExprStmt(Expr::Infix { left: Box::new(Expr::Identifier("x".to_string())), op: Infix::Plus, right: Box::new(Expr::Identifier("y".to_string())) }),
       Stmt::ExprStmt(Expr::Infix { left: Box::new(Expr::Identifier("x".to_string())), op: Infix::Minus, right: Box::new(Expr::Identifier("y".to_string())) }),
       Stmt::ExprStmt(Expr::Infix { left: Box::new(Expr::Identifier("x".to_string())), op: Infix::Multiply, right: Box::new(Expr::Identifier("y".to_string())) }),

       Stmt::LetStatement { ident: "z".to_string(), expr: Expr::Infix { left: Box::new(Expr::Identifier("x".to_string())), op: Infix::Plus, right: Box::new(Expr::Identifier("y".to_string())) } },
       Stmt::LetStatement { ident: "z".to_string(), expr: Expr::Infix { left: Box::new(Expr::Identifier("x".to_string())), op: Infix::Minus, right: Box::new(Expr::Identifier("y".to_string())) } },
       Stmt::LetStatement { ident: "z".to_string(), expr: Expr::Infix { left: Box::new(Expr::Identifier("x".to_string())), op: Infix::Multiply, right: Box::new(Expr::Identifier("y".to_string())) } },
       Stmt::LetStatement { ident: "z".to_string(), expr: Expr::Infix { left: Box::new(Expr::Identifier("x".to_string())), op: Infix::Plus, right: Box::new(Expr::Integer(300)) } },
       Stmt::LetStatement { ident: "z".to_string(), expr: Expr::Infix { left: Box::new(Expr::Identifier("x".to_string())), op: Infix::Minus, right: Box::new(Expr::Integer(300)) } },
       Stmt::LetStatement { ident: "z".to_string(), expr: Expr::Infix { left: Box::new(Expr::Identifier("x".to_string())), op: Infix::Multiply, right: Box::new(Expr::Integer(300)) } },

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
    ];

    assert_eq!(program.stmt, expected);
}
