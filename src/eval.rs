use crate::parser::ast::{Expr, Program, Stmt};
use crate::parser::token::Token;
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Int(i64),
    Bool(bool),
    Nil,
}

use Value::*;

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let output = match self {
            Int(n) => n.to_string(),
            Bool(b) => b.to_string(),
            Nil => "nil".to_string(),
        };

        writeln!(f, "{}", output)
    }
}

pub struct Eval {
    bindings: HashMap<String, Value>,
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
                let value = self.eval_expr(expr);
                self.bindings.insert(ident, value);
                Nil
            }
            Stmt::Assign { op, ident, expr } => {
                let binding = self.bindings.get(&ident);

                if binding.is_none() {
                    return Nil;
                }

                let value = self.eval_expr(expr);
                let binding = self.bindings.get_mut(&ident);

                assert!(op == '\0' || op == '+' || op == '-' || op == '*' || op == '/');

                if op == '\0' {
                    *binding.unwrap() = value;
                } else if let Some(Value::Int(n)) = binding {
                    if let Value::Int(value) = value {
                        match op {
                            '+' => *n += value,
                            '-' => *n -= value,
                            '*' => *n *= value,
                            '/' => {
                                if let Some(value) = n.checked_div(value) {
                                    *n = value;
                                } else {
                                    println!("syntax error");
                                }
                            }
                            _ => unreachable!(),
                        }
                    }
                }

                Nil
            }
            Stmt::Ret(expr) => self.eval_expr(expr),
        }
    }

    fn eval_expr(&mut self, expr: Expr) -> Value {
        match expr {
            Expr::Int(n) => Int(n),
            Expr::Bool(b) => Bool(b),
            Expr::Ident(ident) => self.bindings.get(&ident).unwrap_or(&Value::Nil).clone(),
            Expr::Prefix { op, value } => match op {
                Token::Sub => match self.eval_expr(*value) {
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
                match (self.eval_expr(*left), self.eval_expr(*right)) {
                    (Int(x), Int(y)) => match op {
                        Token::Add => Int(x + y),
                        Token::Sub => Int(x - y),
                        Token::Mul => Int(x * y),
                        Token::Div => {
                            if let Some(value) = x.checked_div(y) {
                                Int(value)
                            } else {
                                println!("syntax error");
                                Nil
                            }
                        }
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
            Expr::If { cond, csq, alt } => {
                if let Bool(b) = self.eval_expr(*cond) {
                    if b {
                        let mut value = Nil;

                        for stmt in csq {
                            value = self.eval_stmt(stmt);
                        }

                        value
                    } else {
                        if let Some(alt) = alt {
                            let mut value = Nil;

                            for stmt in alt {
                                value = self.eval_stmt(stmt);
                            }

                            value
                        } else {
                            Nil
                        }
                    }
                } else {
                    Nil
                }
            }
            _ => Nil,
            // Expr::Fn { args, body } => {
            //     todo!()
            // }
            // Expr::FnCall { ident, args } => {
            //     todo!()
            // },
        }
    }
}

#[test]
fn assign() {
    let input = "let x = 1; x = x + 1; x;";
    let program = crate::parser::Parser::parse(input);
    let mut eval = Eval::new();

    assert_eq!(eval.eval(program), Value::Int(2));
}
