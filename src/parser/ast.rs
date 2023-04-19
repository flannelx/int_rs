#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub stmt: Vec<Stmt>
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Identifier(String),
    Literal(Literal),
    Prefix { op: Prefix, right: Box<Expr> },
    Infix { left: Box<Expr>, op: Infix, right: Box<Expr> },
}

#[derive(PartialEq, Debug, Clone)]
pub enum Prefix {
    Plus,
    Minus,
    Not,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Infix {
    Plus,
    Minus,
    Divide,
    Multiply,
    Equal,
    NotEqual,
    Gte,
    Lte,
    Gt,
    Lt,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i64),
    Bool(bool),
    String(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    LetStatement { ident: String, expr: Expr },
    ReturnStatement(Expr),
    ExprStmt(Expr),
}

#[test]
fn test_display() {
}
