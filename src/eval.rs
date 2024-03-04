use crate::parser::ast::{Expr, Program, Stmt};
use crate::parser::token::Token;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    Bool(bool),
    Fn { args: Vec<String>, body: Vec<Stmt> },
    Nil,
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let output = match self {
            Int(n) => n.to_string(),
            Bool(b) => b.to_string(),
            Nil => "nil".to_string(),
            Fn { args: _, body: _ } => todo!(),
        };

        writeln!(f, "{}", output)
    }
}

use Value::*;

pub struct Eval {
    bindings: HashMap<String, Expr>,
}

impl Eval {
    pub fn new() -> Self {
        Self {
            bindings: HashMap::new(),
        }
    }

    pub fn eval(&mut self, program: Program) -> Value {
        if !program.errors().is_empty() {
            println!("{:?}", program.errors());
        }

        let mut value = Value::Nil;

        for stmt in program.stmts() {
            value = self.eval_stmt(stmt.clone());
        }

        value
    }

    fn eval_stmt(&mut self, stmt: Stmt) -> Value {
        match stmt {
            Stmt::Expr(expr) => self.eval_expr(expr),
            Stmt::Let { ident, expr } => {
                self.bindings.insert(ident, expr);
                Nil
            }
            Stmt::Assign { ident, expr } => {
                if let Some(value) = self.bindings.get_mut(&ident) {
                    *value = expr;
                }
                Nil
            }
            Stmt::Ret(expr) => self.eval_expr(expr),
        }
    }
     
    fn eval_expr(&self, expr: Expr) -> Value {
        dbg!(&expr);
        let value = match expr {
            Expr::Int(n) => Int(n),
            Expr::Bool(b) => Bool(b),
            Expr::Ident(ident) => self
                .bindings
                .get(&ident)
                .map_or(Value::Nil, |expr| self.eval_expr(expr.clone())),
            Expr::Prefix { op, value } => match op {
                Token::Minus => match self.eval_expr(*value) {
                    Int(n) => Int(-n),
                    _ => Nil,
                },
                Token::Bang => match self.eval_expr(*value) {
                    Bool(b) => Bool(!b),
                    _ => Nil,
                },
                _ => unreachable!(),
            },
            Expr::Infix { op, left, right } => {
                let left = self.eval_expr(*left);
                let right = self.eval_expr(*right);

                match (left, right) {
                    (Int(x), Int(y)) => match op {
                        Token::Plus => Int(x + y),
                        Token::Minus => Int(x - y),
                        Token::Asterisk => Int(x * y),
                        Token::Slash => Int(x / y),
                        Token::Eq => Bool(x == y),
                        Token::NotEq => Bool(x != y),
                        Token::Lt => Bool(x < y),
                        Token::Gt => Bool(x > y),
                        _ => Nil,
                    },
                    (Bool(a), Bool(b)) => match op {
                        Token::Eq => Bool(a == b),
                        Token::NotEq => Bool(a != b),
                        _ => Nil,
                    },
                    _ => Nil,
                }
            }
            _ => Nil,
            // Expr::If { cond, csq, alt } => {
            //     todo!()
            // },
            // Expr::Fn { args, body } => {
            //     todo!()
            // }
            // Expr::FnCall { ident, args } => {
            //     todo!()
            // },
        };

        dbg!(&value);
        value
    }
}
