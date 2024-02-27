use super::token::Token;

#[derive(Debug)]
pub struct Program {
    stmts: Vec<Stmt>,
    errors: Vec<String>,
}

impl Program {
    pub fn new() -> Self {
        Self {
            stmts: Vec::new(),
            errors: Vec::new(),
        }
    }

    pub fn stmts(&self) -> &Vec<Stmt> {
        &self.stmts
    }

    pub fn errors(&self) -> &Vec<String> {
        &self.errors
    }

    pub fn push_stmt(&mut self, stmt: Stmt) {
        self.stmts.push(stmt);
    }

    pub fn push_error(&mut self, error: String) {
        self.errors.push(error);
    }
}

impl ToString for Program {
    fn to_string(&self) -> String {
        let stmts = self
            .stmts
            .iter()
            .map(|stmt| stmt.to_string())
            .collect::<Vec<_>>()
            .join("");

        stmts
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Let(Expr, Expr),
    Ret(Expr),
    Expr(Expr),
    Block(Vec<Stmt>),
}

impl ToString for Stmt {
    fn to_string(&self) -> String {
        match self {
            Self::Let(name, value) => {
                format!("let {} = {};", name.to_string(), value.to_string())
            }
            Self::Ret(value) => {
                format!("return {};", value.to_string())
            }
            Self::Expr(expr) => expr.to_string(),
            Self::Block(stmts) => stmts
                .iter()
                .map(|stmt| stmt.to_string())
                .collect::<Vec<_>>()
                .join(""),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Ident(String),
    Int(i64),
    Bool(bool),
    Prefix(Token, Box<Expr>),
    Infix(Token, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Stmt>, Option<Box<Stmt>>),
    Call(String, Vec<Expr>),
}

impl ToString for Expr {
    fn to_string(&self) -> String {
        match self {
            Self::Ident(name) => name.to_string(),
            Self::Int(n) => n.to_string(),
            Self::Prefix(op, expr) => {
                format!("({}{})", op.to_string(), expr.to_string())
            }
            Self::Infix(op, l, r) => {
                format!("({} {} {})", l.to_string(), op.to_string(), r.to_string())
            }
            Self::Bool(b) => b.to_string(),
            Self::If(cond, conseq, alt) => {
                let mut output = format!(
                    "if {} {{ {} }}",
                    cond.to_string(),
                    conseq.to_string(),
                ); 

                if let Some(alt) = alt {
                    output.push_str(&format!("else {{{}}}", alt.to_string()));
                }

                output
            }
            Self::Call(func, args) => {
                format!(
                    "{}({})",
                    func,
                    args.iter()
                        .map(|arg| arg.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn to_string_works() {
        let program = Program {
            stmts: vec![Stmt::Let(
                Expr::Ident("a".to_string()),
                Expr::Ident("b".to_string()),
            )],
            errors: vec![],
        };

        assert_eq!(program.to_string(), "let a = b;".to_string());
    }
}
