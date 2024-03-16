use super::Env;
use crate::parser::Stmt;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub enum Obj {
    Int(i64),
    Bool(bool),
    Return(Box<Obj>),
    Nil,
    Error(String),
    Function {
        params: Vec<String>,
        body: Vec<Stmt>,
        outer_env: Rc<RefCell<Env>>,
    },
}

impl std::fmt::Display for Obj {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let output = match self {
            Obj::Int(n) => n.to_string(),
            Obj::Bool(b) => b.to_string(),
            Obj::Return(value) => value.to_string(),
            Obj::Function { params, .. } => format!("Fn({})", params.join(", ")),
            Obj::Error(s) => s.clone(),
            Obj::Nil => "Nil".to_string(),
        };

        write!(f, "{}", output)
    }
}
