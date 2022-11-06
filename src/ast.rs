use crate::token::{Token, TokenType};

#[derive(Default, Debug, Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub token: Token,
    pub value: String,
}

#[derive(Debug, Clone)]
pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
}

#[derive(Debug, Clone)]
pub struct LetStatement {
    pub token: Token,
    pub identifier: Identifier,
    pub expression: Expression,
}

#[derive(Debug, Clone)]
pub struct ReturnStatement {
    pub token: Token,
    pub expression: Expression,
}

impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for stmt in self.statements.iter() {
            match stmt {
                Statement::LetStatement(s) => write!(
                    f,
                    "{} {} = {};",
                    s.token.literal, s.identifier.value, s.expression.value
                )?,
                Statement::ReturnStatement(s) => {
                    write!(f, "{} {};", s.token.literal, s.expression.value)?
                }
            }
        }
        Ok(())
    }
}

#[test]
fn test_display() {
    let mut program = Program { statements: vec![] };
    program.statements.push(
        Statement::LetStatement(
            LetStatement {
                token: Token { typ: TokenType::LET, literal: "let".to_string() },
                identifier: Identifier { token: Token { typ: TokenType::IDENT, literal: "myVar".to_string() }, value: "myVar".to_string() },
                expression: Expression { token: Token { typ: TokenType::IDENT, literal: "anotherVar".to_string() }, value: "anotherVar".to_string() }
            }
    ));
    assert_eq!(format!("{program}"), "let myVar = anotherVar;");
}
