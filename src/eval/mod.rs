mod env;

use self::env::Env;
use crate::parser::{
    ast::{Expr, Infix, Literal, Prefix, Program, Stmt},
    Parser,
};
use anyhow::bail;
use std::{cell::RefCell, fmt::Display, rc::Rc};

macro_rules! otv {
    ($o: expr, $t: ident) => {
        if let Object::$t(ret) = $o {
            ret
        } else {
            bail!("Expected {:?}, got {:?}", stringify!($t), $o)
        }
    };
}

#[derive(PartialEq, Debug, Clone)]
pub enum Object {
    Integer(i64),
    Bool(bool),
    Return(Box<Object>),
    Function { params: Vec<String>, body: Program },
    String(String),
    Null,

    // TODO:
    Builtin,
    Array,
    Hash,
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Integer(i) => write!(f, "{}", i),
            Object::Bool(b) => write!(f, "{}", b),
            Object::Return(o) => o.fmt(f),
            Object::String(s) => write!(f, "{}", s),
            o => write!(f, "{:?}", o),
        }
    }
}

#[derive(Debug)]
pub struct Evaluator {
    env: Rc<RefCell<Env>>,
}

impl Evaluator {
    pub fn new() -> Self {
        Self {
            env: Rc::new(RefCell::new(Env::new())),
        }
    }

    pub fn eval_block_stmt(&mut self, mut program: Program) -> anyhow::Result<Object> {
        match program.len() {
            0 => Ok(Object::Null),
            _ => {
                let obj = self.eval_stmt(program.pop_front().unwrap())?;
                if matches!(obj, Object::Return(_)) || program.len() == 0 {
                    return Ok(obj);
                }
                self.eval_block_stmt(program)
            }
        }
    }

    pub fn eval_stmt(&mut self, stmt: Stmt) -> anyhow::Result<Object> {
        use Stmt::*;
        Ok(match stmt {
            LetStatement { ident, expr } => {
                let obj = self.eval_expr(expr)?;
                self.register_identifier(ident, obj)
            }
            ReturnStatement(expr) => Object::Return(Box::new(self.eval_expr(expr)?)),
            ExprStmt(expr) => self.eval_expr(expr)?,
        })
    }

    pub fn eval_expr(&mut self, expr: Expr) -> anyhow::Result<Object> {
        match expr {
            Expr::Identifier(ident) => self.eval_identifier(ident),
            Expr::Literal(literal) => Ok(self.eval_literal(literal)),
            Expr::Prefix { op, right } => self.eval_prefix(&op, *right),
            Expr::Infix { left, op, right } => self.eval_infix(*left, op, *right),
            Expr::If { cond, body, alt } => self.eval_if(cond, body, alt),
            Expr::Function {
                ident,
                params,
                body,
            } => Ok(self.eval_fn(ident, params, body)),
            Expr::Call { func, args } => self.eval_fn_call(func, args),
        }
    }

    pub fn register_identifier(&mut self, ident: String, obj: Object) -> Object {
        self.env.borrow_mut().set(&ident, obj.clone());
        obj
    }

    pub fn eval_identifier(&mut self, ident: String) -> anyhow::Result<Object> {
        if let Some(obj) = self.env.borrow().get(&ident) {
            Ok(obj)
        } else {
            bail!("Identifier not found {ident}")
        }
    }

    pub fn eval_literal(&mut self, literal: Literal) -> Object {
        use Literal::*;
        match literal {
            Int(i) => Object::Integer(i),
            Bool(b) => Object::Bool(b),
            String(s) => Object::String(s),
        }
    }

    pub fn eval_prefix(&mut self, op: &Prefix, expr: Expr) -> anyhow::Result<Object> {
        let obj = self.eval_expr(expr)?;
        Ok(match *op {
            Prefix::Plus => Object::Integer(otv!(obj, Integer)),
            Prefix::Minus => Object::Integer(-otv!(obj, Integer)),
            Prefix::Not => Object::Bool(!otv!(obj, Bool)),
        })
    }

    pub fn eval_infix(&mut self, left: Expr, op: Infix, right: Expr) -> anyhow::Result<Object> {
        if matches!(op, Infix::Plus | Infix::Equal | Infix::NotEqual) {
            return Ok(match op {
                Infix::Plus => match (self.eval_expr(left)?, self.eval_expr(right)?) {
                    (Object::Integer(l), Object::Integer(r)) => Object::Integer(l + r),
                    (Object::Integer(l), Object::String(r)) => Object::String(l.to_string() + &r),
                    (Object::String(l), Object::Integer(r)) => Object::String(l + &r.to_string()),
                    (Object::String(l), Object::String(r)) => Object::String(l + &r),
                    (l, r) => {
                        bail!("Incompatible type to perform {op:?}, left: {l:?}, right: {r:?}")
                    }
                },
                Infix::Equal => match (self.eval_expr(left)?, self.eval_expr(right)?) {
                    (Object::Integer(l), Object::Integer(r)) => Object::Bool(l == r),
                    (Object::String(l), Object::String(r)) => Object::Bool(l == r),
                    (l, r) => {
                        bail!("Incompatible type to perform {op:?}, left: {l:?}, right: {r:?}")
                    }
                },
                Infix::NotEqual => match (self.eval_expr(left)?, self.eval_expr(right)?) {
                    (Object::Integer(l), Object::Integer(r)) => Object::Bool(l != r),
                    (Object::String(l), Object::String(r)) => Object::Bool(l != r),
                    (l, r) => {
                        bail!("Incompatible type to perform {op:?}, left: {l:?}, right: {r:?}")
                    }
                },
                _ => unreachable!(),
            });
        }
        let l = otv!(self.eval_expr(left.to_owned())?, Integer);
        let r = otv!(self.eval_expr(right.to_owned())?, Integer);
        #[cfg_attr(rustfmt, rustfmt_skip)]
        Ok(match op {
            Infix::Minus    => Object::Integer(l - r),
            Infix::Divide   => Object::Integer(l / r),
            Infix::Multiply => Object::Integer(l * r),
            Infix::Gte      => Object::Bool(l >= r),
            Infix::Lte      => Object::Bool(l <= r),
            Infix::Gt       => Object::Bool(l > r),
            Infix::Lt       => Object::Bool(l < r),
            _ => unreachable!(),
        })
    }

    pub fn eval_if(
        &mut self,
        cond: Box<Expr>,
        body: Program,
        alt: Option<Program>,
    ) -> anyhow::Result<Object> {
        let ret = self.eval_expr(*cond)?;
        if let Object::Bool(b) = ret {
            if b {
                self.eval_block_stmt(body)
            } else {
                if alt.is_none() {
                    return Ok(Object::Null);
                }
                self.eval_block_stmt(alt.unwrap())
            }
        } else {
            bail!("Expected conditions to evaluated as bool, got {:?}", ret)
        }
    }

    pub fn eval_fn(&mut self, ident: String, params: Vec<String>, body: Program) -> Object {
        let ret = Object::Function { params, body };
        self.env.borrow_mut().set(&ident, ret.clone());
        ret
    }

    pub fn eval_fn_call(&mut self, ident: String, args: Vec<Expr>) -> anyhow::Result<Object> {
        let mut obj_args = Vec::new();
        for a in args.iter() {
            obj_args.push(self.eval_expr(a.to_owned())?);
        }
        if let Some(obj) = self.env.borrow_mut().get(&ident) && let Object::Function{ params, body } = obj && obj_args.len() == params.len() {
            let mut eval = Evaluator::new();
            for (o, p) in obj_args.iter().zip(params.iter()) {
                eval.env.borrow_mut().set(p, o.to_owned());
            }
            eval.eval_block_stmt(body)
        }
        else{
            bail!("Function {ident} not found");
        }
    }
}

#[test]
fn eval_test() {
    let input = r#"
    let x = 1;
    let y = 2;
    let z = "x + y = " + x + y;
    let hello = "Hello ";
    fn add(x, y) {
        x + y;
    };
    add(hello, "World! z is " + z);
    "#;
    let mut p = Parser::new(input);
    let program = p.parse_program().unwrap();
    let mut eval = Evaluator::new();
    let ret = eval.eval_block_stmt(program).unwrap();
    assert_eq!(format!("{ret}"), "Hello World! z is x + y = 3");
}
