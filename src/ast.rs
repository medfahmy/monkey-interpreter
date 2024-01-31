#[derive(Debug)]
pub struct Program {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Let(Expr, Expr),
    Ret(Expr),
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Ident(String),
    Value,
}
