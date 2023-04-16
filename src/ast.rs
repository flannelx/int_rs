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
    LetStatement {
        token: Token,
        identifier: Identifier,
        expression: Expression,
    },
    ReturnStatement {
        token: Token,
        expression: Expression,
    },
}

impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for stmt in self.statements.iter() {
            match stmt {
                Statement::LetStatement {
                    token,
                    identifier,
                    expression,
                } => write!(
                    f,
                    "{} {} = {};",
                    token.literal, identifier.value, expression.value
                )?,
                Statement::ReturnStatement { token, expression } => {
                    write!(f, "{} {};", token.literal, expression.value)?
                }
            }
        }
        Ok(())
    }
}

#[test]
fn test_display() {
    let mut program = Program { statements: vec![] };
    program
        .statements
        .push(Statement::LetStatement {
            token: Token {
                typ: TokenType::LET,
                literal: "let".to_string(),
            },
            identifier: Identifier {
                token: Token {
                    typ: TokenType::IDENT,
                    literal: "myVar".to_string(),
                },
                value: "myVar".to_string(),
            },
            expression: Expression {
                token: Token {
                    typ: TokenType::IDENT,
                    literal: "anotherVar".to_string(),
                },
                value: "anotherVar".to_string(),
            },
        });
    assert_eq!(format!("{program}"), "let myVar = anotherVar;");
}
