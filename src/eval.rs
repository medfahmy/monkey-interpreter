#![allow(unused, warnings)]

use crate::parser::{Expr, Program, Stmt, Token};

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Int(i64),
    Bool(bool),
    Ret(Box<Object>),
    Nil,
}

use Object::*;

impl std::fmt::Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let output = match self {
            Int(n) => n.to_string(),
            Bool(b) => b.to_string(),
            Ret(value) => value.to_string(),
            Nil => "nil".to_string(),
        };

        writeln!(f, "{}", output)
    }
}

pub trait Eval {
    fn eval(&self) -> Object;
}

impl Eval for Program {
    fn eval(&self) -> Object {
        if self.stmts().is_empty() || !self.errors().is_empty() {
            return Nil;
        }

        let mut value = Nil;

        for stmt in self.stmts() {
            if let Ret(obj) = stmt.eval() {
                return *obj;
            } else {
                value = stmt.eval();
            }
        }

        value
    }
}

impl Eval for Vec<Stmt> {
    fn eval(&self) -> Object {
        let mut value = Nil;

        for stmt in self {
            if let Ret(obj) = stmt.eval() {
                return *obj;
            } else {
                value = stmt.eval();
            }
        }

        value
    }
}

impl Eval for Stmt {
    fn eval(&self) -> Object {
        match self {
            Self::Expr(expr) => expr.eval(),
            Self::Ret(expr) => Object::Ret(Box::new(expr.eval())),
            _ => Nil,
        }
    }
}

impl Eval for Expr {
    fn eval(&self) -> Object {
        match self {
            Self::Int(n) => Int(*n),
            Self::Bool(b) => Bool(*b),
            Self::Prefix { op, value } => match op {
                Token::Bang => {
                    if let Bool(b) = value.eval() {
                        Bool(!b)
                    } else {
                        Nil
                    }
                }
                Token::Sub => {
                    if let Int(n) = value.eval() {
                        Int(-n)
                    } else {
                        Nil
                    }
                }
                _ => unreachable!(),
            },
            Self::Infix { op, left, right } => match op {
                Token::Add | Token::Sub | Token::Mul | Token::Div => {
                    if let (Int(x), Int(y)) = (left.eval(), right.eval()) {
                        match op {
                            Token::Add => Int(x + y),
                            Token::Sub => Int(x - y),
                            Token::Mul => Int(x * y),
                            Token::Div => Int(x / y),
                            _ => unreachable!(),
                        }
                    } else {
                        Nil
                    }
                }
                Token::Eq | Token::NotEq | Token::Gt | Token::Lt => {
                    if let (Bool(x), Bool(y)) = (left.eval(), right.eval()) {
                        match op {
                            Token::Eq => Bool(x == y),
                            Token::NotEq => Bool(x != y),
                            Token::Lt => Bool(x < y),
                            Token::Gt => Bool(x > y),
                            _ => unreachable!(),
                        }
                    } else {
                        Nil
                    }
                }
                _ => unreachable!(),
            },
            Self::Ident(ident) => todo!(),
            Self::If { cond, csq, alt } => {
                if let Bool(cond) = cond.eval() {
                    if cond {
                        csq.eval()
                    } else {
                        alt.eval()
                    }
                } else {
                    Nil
                }
            }
            _ => Nil,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Parser;
    use Object::*;

    fn eval(input: &str) -> Object {
        let program = Parser::parse(input);
        program.eval()
    }

    #[test]
    fn int() {
        let inputs = [
            "4",
            "10",
            "12 + 12",
            "10 - 5",
            "12 * 12",
            "10 / 5",
            "2 * (2 - 2)",
            "4 / (2 + 2)",
        ];
        let outputs = [4, 10, 24, 5, 144, 2, 0, 1];

        for i in 0..3 {
            assert_eq!(eval(inputs[i]), Int(outputs[i]));
        }
    }

    #[test]
    fn bool() {
        let inputs = ["true", "false", "!true", "!false", "!!true", "!!false"];
        let outputs = [true, false, false, true, true, false];

        for i in 0..2 {
            assert_eq!(eval(inputs[i]), Bool(outputs[i]));
        }
    }

    #[test]
    fn if_else() {
        let inputs = [
            "if (true) { 10 }",
            "if (false) { 10 }",
            "if (1 < 2) { 10 }",
            "if (1 > 2) { 10 }",
            "if (1 < 2) { 10 } else { 20 }",
            "if (1 > 2) { 10 } else { 20 }",
        ];

        let outputs = [
            Int(10),
            Nil,
            Int(10),
            Nil,
            Int(10),
            Int(20),
        ];

        for i in 0..2 {
            assert_eq!(eval(inputs[i]), outputs[i]);
        }
    }

    #[test]
    fn ret() {
        let inputs = [
            "return 10;",
            "return 10; 9;",
            "return 2 * 5; 9;",
            "9; return 2 * 5; 9;",
        ];

        for i in 0..2 {
            assert_eq!(eval(inputs[i]), Int(10));
        }
    }

    #[test]
    fn if_ret() {
        let input = "if (true) { if (true) { return 1; } return 0; }";
        assert_eq!(eval(input), Int(1));
    }
}
