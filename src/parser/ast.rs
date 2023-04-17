#[derive(Debug, Clone)]
pub struct Program {
    stmt: Vec<Stmt>
}

#[derive(Debug, Clone)]
pub enum Expr {
    Identifier(String),
    Literal(Literal),
    Prefix { prefix: Prefix, expr: Box<Expr> },
    Infix { infix: Infix, expr: Box<Expr> },
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
    GreaterThanEqual,
    LessThanEqual,
    GreaterThan,
    LessThan,
}

#[derive(Debug, Clone)]
pub enum Literal {
    Int(i64),
    Bool(bool),
    String(String),
}

#[derive(Debug, Clone)]
pub enum Stmt {
    LetStatement { ident: String, expr: Expr },
    ReturnStatement(Expr),
    ExprStmt(Expr),
}

#[test]
fn test_display() {
}
