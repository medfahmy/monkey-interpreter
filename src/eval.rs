use crate::parser::{Expr, Program, Stmt, Token};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Env {
    bindings: HashMap<String, Object>,
    outer: Option<Box<Env>>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            bindings: HashMap::new(),
            outer: None,
        }
    }

    fn extend(&self) -> Self {
        Self {
            bindings: HashMap::new(),
            outer: Some(Box::new(self.clone())),
        }
    }

    fn get(&self, ident: &String) -> Option<Object> {
        self.bindings.get(ident).cloned()
    }

    fn set(&mut self, ident: String, obj: Object) -> Object {
        self.bindings.insert(ident, obj.clone());
        obj
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Int(i64),
    Bool(bool),
    Return(Box<Object>),
    Nil,
    Error(String),
    Function {
        params: Vec<String>,
        body: Vec<Stmt>,
        local_env: Env,
    },
}

use Object::*;

impl std::fmt::Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let output = match self {
            Int(n) => n.to_string(),
            Bool(b) => b.to_string(),
            Return(value) => value.to_string(),
            Function {
                params,
                body,
                local_env: _,
            } => {
                format!(
                    "fn({}) {{\n\t {}\n}}",
                    params.join(", "),
                    body.iter()
                        .map(|arg| arg.to_string())
                        .collect::<Vec<_>>()
                        .join("\n\t")
                )
            }
            Error(s) => s.clone(),
            Nil => "Nil".to_string(),
        };

        writeln!(f, "{}", output)
    }
}

pub trait Eval {
    fn eval(&self, env: &mut Env) -> Object;
}

impl Eval for Program {
    fn eval(&self, env: &mut Env) -> Object {
        if self.stmts().is_empty() || !self.errors().is_empty() {
            return Nil;
        }

        let mut value = Nil;

        for stmt in self.stmts() {
            value = stmt.eval(env);

            if let Return(obj) = value {
                return *obj;
            }

            match value {
                Return(obj) => return *obj,
                Error(_) => return value,
                _ => {}
            }
        }

        value
    }
}

impl Eval for Vec<Stmt> {
    fn eval(&self, env: &mut Env) -> Object {
        let mut value = Nil;

        for stmt in self {
            value = stmt.eval(env);

            match value {
                Return(_) | Error(_) => return value,
                _ => {}
            }
        }

        value
    }
}

impl Eval for Stmt {
    fn eval(&self, env: &mut Env) -> Object {
        match self {
            Self::Expr(expr) => expr.eval(env),
            Self::Ret(expr) => match expr.eval(env) {
                Error(s) => Error(s),
                value => Return(Box::new(value)),
            },
            Self::Let { ident, expr } => match expr.eval(env) {
                Error(s) => Error(s),
                value => env.set(ident.to_string(), value),
            },
            _ => unimplemented!(),
        }
    }
}

impl Eval for Expr {
    fn eval(&self, env: &mut Env) -> Object {
        match self {
            Self::Int(n) => Int(*n),
            Self::Bool(b) => Bool(*b),
            Self::Prefix { op, expr } => match expr.eval(env) {
                Error(s) => Error(s),
                Bool(b) => match op {
                    Token::Bang => Bool(!b),
                    Token::Sub => Error("unknown operator: -Bool".to_string()),
                    _ => unreachable!(),
                },
                Int(n) => match op {
                    Token::Sub => Int(-n),
                    Token::Bang => Error("unknown operator: !Int".to_string()),
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            },
            Self::Infix { op, left, right } => match (left.eval(env), right.eval(env)) {
                (Error(s), _) => Error(s),
                (_, Error(s)) => Error(s),
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
                    Token::Eq => Bool(x == y),
                    Token::NotEq => Bool(x != y),
                    Token::Lt => Bool(x < y),
                    Token::Gt => Bool(x > y),
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            },
            Self::If { cond, csq, alt } => match cond.eval(env) {
                Error(s) => Error(s),
                Bool(cond) => {
                    if cond {
                        csq.eval(env)
                    } else {
                        alt.eval(env)
                    }
                }
                _ => unreachable!(),
            },
            Self::Ident(ident) => {
                if let Some(value) = env.get(ident) {
                    value.clone()
                } else {
                    // println!("{:?}", env);
                    Error(format!("identifier not found: '{}'", ident))
                }
            }
            Self::Fn { params, body } => {
                let func = Function {
                    params: params.clone(),
                    body: body.clone(),
                    local_env: env.clone(),
                };

                // println!("{:?}", func);

                func
            }
            Self::FnCall { ident, args } => {
                if let Some(obj) = env.get(ident) {
                    if let Function { params, body, local_env } = obj {
                        if args.len() != params.len() {
                            return Error(format!(
                                "function '{}' expected {} parameters, got {}",
                                ident,
                                params.len(),
                                args.len()
                            ));
                        }

                        let mut local_env = local_env;
                        let mut values = Vec::new();

                        for arg in args {
                            match arg.eval(&mut local_env) {
                                Error(s) => return Error(s),
                                obj => values.push(obj),
                            }
                        }

                        for i in 0..args.len() {
                            local_env.set(params[i].clone(), values[i].clone());
                        }

                        println!("{:?}", local_env);

                        match body.eval(&mut local_env) {
                            Return(obj) => *obj,
                            obj => obj,
                        }
                    } else {
                        Error(format!("identifier is not a function: '{}'", ident))
                    }
                } else {
                    Error(format!("identifier not found: '{}'", ident))
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Parser;

    fn eval(input: &str) -> Object {
        let program = Parser::parse(input);
        let mut env = Env::new();
        program.eval(&mut env)
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
            assert_eq!(
                eval(inputs[i]),
                Error(outputs[i].to_string()),
                "input: {}",
                inputs[i]
            );
        }
    }

    #[test]
    fn let_ident() {
        let inputs = [
            "let x = 4; x;",
            "let x = 2 * 2; x;",
            "let x = 4; let y = x; y;",
            "let x = 2; let y = x; let z = x + y; z",
        ];

        for input in inputs {
            assert_eq!(eval(input), Int(4));
        }

        assert_eq!(
            eval("foobar"),
            Error("identifier not found: 'foobar'".to_string())
        );
    }

    #[test]
    fn func() {
        let input = "fn(x) { x + 2; };";
        match eval(input) {
            Function {
                params,
                body,
                local_env: _,
            } => {
                assert_eq!(params, vec!["x"]);
                assert_eq!(body[0].to_string(), "(x + 2)");
            }
            v => panic!("expected function, got '{:?}'", v),
        }
    }

    #[test]
    fn fn_call() {
        let inputs = [
            "let id = fn(x) { x; }; id(20);",
            "let id = fn(x) { return x; }; id(20);",
            "let double = fn(x) { return x * 2; }; double(10);",
            "let add = fn(x, y) { return x + y; }; add(10, 10);",
            "let add = fn(x, y) { return x + y; }; add(5, 5) + add(5, 5);",
        ];

        for input in inputs {
            assert_eq!(eval(input), Int(20), "{}", input);
        }
    }

    #[test]
    fn closure() {
        let input = r#"
            let create_adder = fn(x) { 
                fn(y) { x + y }; 
            };

            let add_two = create_adder(2);
            add_two(2);
        "#;

        assert_eq!(eval(input), Int(4));
    }
}
