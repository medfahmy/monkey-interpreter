#![allow(unused, warnings)]

use crate::parser::{Expr, Program, Stmt, Token};

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Int(i64),
    Bool(bool),
    Ret(Box<Object>),
    Nil,
    Error(String),
}

use Object::*;

// struct RuntimeError {
//     message: String,
//     line: usize,
//     row: usize,
//     stacktrace: Any,
// }

impl std::fmt::Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let output = match self {
            Int(n) => n.to_string(),
            Bool(b) => b.to_string(),
            Ret(value) => value.to_string(),
            Nil => "Nil".to_string(),
            Error(s) => s.clone(),
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
            value = stmt.eval();

            if let Ret(obj) = value {
                return *obj;
            }

            match value {
                Ret(obj) => return *obj,
                Error(_) => return value,
                _ => {},
            }
        }

        value
    }
}

impl Eval for Vec<Stmt> {
    fn eval(&self) -> Object {
        let mut value = Nil;

        for stmt in self {
            value = stmt.eval();

            match value {
                Ret(_) | Error(_) => return value,
                _ => {},
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
            Self::Prefix { op, expr } => match op {
                Token::Bang => match expr.eval() {
                    Bool(b) => Bool(!b),
                    Int(_) => Error("unknown operator: !Int".to_string()),
                    _ => unreachable!(),
                },
                Token::Sub => match expr.eval() {
                    Int(n) => Int(-n),
                    Bool(_) => Error("unknown operator: -Bool".to_string()),
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            },
            Self::Infix { op, left, right } => match op {
                Token::Add | Token::Sub | Token::Mul | Token::Div => {
                    let (left, right) = (left.eval(), right.eval());

                    match (left, right) {
                        (Bool(_), Bool(_)) => {
                            Error(format!("unknown operator: Bool {} Bool", op.to_string()))
                        }
                        (Bool(_), _) | (_, Bool(_)) => {
                            Error(format!("type mismatch: Int {} Bool", op.to_string()))
                        }
                        (Int(x), Int(y)) => match op {
                            Token::Add => Int(x + y),
                            Token::Sub => Int(x - y),
                            Token::Mul => Int(x * y),
                            Token::Div => Int(x / y),
                            _ => unreachable!(),
                        },
                        _ => unreachable!(),
                    }
                }
                Token::Eq | Token::NotEq | Token::Gt | Token::Lt => {
                    let (left, right) = (left.eval(), right.eval());

                    match (left, right) {
                        (Bool(_), Bool(_)) => {
                            Error(format!("unknown operator: Bool {} Bool", op.to_string()))
                        }
                        (Bool(_), Int(_)) | (Int(_), Bool(_)) => {
                            Error(format!("type mismatch: Bool {} Bool", op.to_string()))
                        }
                        (Int(x), Int(y)) => match op {
                            Token::Eq => Bool(x == y),
                            Token::NotEq => Bool(x != y),
                            Token::Lt => Bool(x < y),
                            Token::Gt => Bool(x > y),
                            _ => unreachable!(),
                        },
                        _ => unreachable!(),
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
    // #[ignore]
    fn if_else() {
        let inputs = [
            "if (true) { 10 }",
            "if (false) { 10 }",
            "if (1 < 2) { 10 }",
            "if (1 > 2) { 10 }",
            "if (1 < 2) { 10 } else { 20 }",
            "if (1 > 2) { 10 } else { 20 }",
        ];

        let outputs = [Int(10), Nil, Int(10), Nil, Int(10), Int(20)];

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
    // #[ignore]
    fn if_ret() {
        let input = "if (true) { if (true) { return 1; } return 0; }";
        assert_eq!(eval(input), Int(1));
    }

    #[test]
    fn errors() {
        let inputs = [
            "5 + true;",
            "5 + true; -5;",
            "!5",
            "-true;",
            "true + false;",
            "5; true + false; 5;",
            "if (10 > 1) { true + false; }",
            "if (10 > 1) { if (10 > 1) { return true + false; } return 1; }",
        ];

        let outputs = [
            "type mismatch: Int + Bool",
            "type mismatch: Int + Bool",
            "unknown operator: !Int",
            "unknown operator: -Bool",
            "unknown operator: Bool + Bool",
            "unknown operator: Bool + Bool",
            "unknown operator: Bool + Bool",
            "unknown operator: Bool + Bool",
        ];

        for i in 0..inputs.len() {
            assert_eq!(eval(inputs[i]), Error(outputs[i].to_string()), "input: {}", inputs[i]);
        }
    }
}
