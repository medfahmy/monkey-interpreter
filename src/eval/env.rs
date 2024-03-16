use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Display;
use std::rc::Rc;

use super::Obj;

#[derive(Debug, Clone, PartialEq)]
pub struct Env {
    bindings: HashMap<String, Obj>,
    outer: Option<Rc<RefCell<Env>>>,
}

impl Env {
    pub fn new() -> Rc<RefCell<Env>> {
        Rc::new(RefCell::new(Self {
            bindings: HashMap::new(),
            outer: None,
        }))
    }

    pub fn extend(outer: &Rc<RefCell<Self>>) -> Rc<RefCell<Env>> {
        Rc::new(RefCell::new(Self {
            bindings: HashMap::new(),
            outer: Some(Rc::clone(outer)),
        }))
    }

    pub fn get(&self, ident: &String) -> Option<Obj> {
        match self.bindings.get(ident) {
            Some(obj) => Some(obj.clone()),
            None => self
                .outer
                .as_ref()
                .and_then(|outer| outer.borrow().get(ident)),
        }
    }

    pub fn set(&mut self, ident: String, obj: Obj) {
        self.bindings.insert(ident, obj);
    }
}

impl Display for Env {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let bindings: Vec<String> = self
            .bindings
            .iter()
            .map(|(key, value)| format!("{} = {}", key, value))
            .collect();

        let outer = match &self.outer {
            None => "[]".to_string(),
            Some(outer) => outer.borrow().to_string(),
        };

        write!(
            f,
            "Env [ bindings: [{}], outer: {} ]",
            bindings.join(", "),
            outer,
        )
    }
}
