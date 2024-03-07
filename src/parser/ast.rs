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
    Let { ident: String, expr: Expr },
    Assign { op: char, ident: String, expr: Expr },
    Ret(Expr),
    Expr(Expr),
}

impl ToString for Stmt {
    fn to_string(&self) -> String {
        match self {
            Self::Let { ident, expr } => {
                format!("let {} = {};", ident.to_string(), expr.to_string())
            }
            Self::Assign { op, ident, expr } => {
                format!("{} = {}{};", ident.to_string(), op, expr.to_string())
            }
            Self::Ret(value) => {
                format!("return {};", value.to_string())
            }
            Self::Expr(expr) => expr.to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Ident(String),
    Int(i64),
    Bool(bool),
    Prefix {
        op: Token,
        value: Box<Expr>,
    },
    Infix {
        op: Token,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    If {
        cond: Box<Expr>,
        csq: Vec<Stmt>,
        alt: Vec<Stmt>,
    },
    Fn {
        args: Vec<Expr>,
        body: Vec<Stmt>,
    },
    FnCall {
        ident: String,
        args: Vec<Expr>,
    },
}

impl ToString for Expr {
    fn to_string(&self) -> String {
        match self {
            Self::Ident(name) => name.to_string(),
            Self::Int(n) => n.to_string(),
            Self::Prefix { op, value } => {
                format!("({}{})", op.to_string(), value.to_string())
            }
            Self::Infix { op, left, right } => {
                format!(
                    "({} {} {})",
                    left.to_string(),
                    op.to_string(),
                    right.to_string()
                )
            }
            Self::Bool(b) => b.to_string(),
            Self::If { cond, csq, alt } => {
                let mut output = format!(
                    "if {} {{\n\t{}\n}}",
                    cond.to_string(),
                    // csq.to_string(),
                    csq.iter()
                        .map(|stmt| stmt.to_string())
                        .collect::<Vec<_>>()
                        .join(""),
                );

                if !alt.is_empty() {
                    output.push_str(&format!(
                        " else {{\n\t{}\n}}",
                        // alt.to_string()
                        alt.iter()
                            .map(|stmt| stmt.to_string())
                            .collect::<Vec<_>>()
                            .join(""),
                    ));
                }

                output
            }
            Self::Fn { args, body } => {
                format!(
                    "fn({}) {{\n\t {}\n }}",
                    args.iter()
                        .map(|arg| arg.to_string())
                        .collect::<Vec<_>>()
                        .join(", "),
                    body.iter()
                        .map(|arg| arg.to_string())
                        .collect::<Vec<_>>()
                        .join("\n\t")
                )
            }
            Self::FnCall { ident, args } => {
                format!(
                    "{}({})",
                    ident,
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
            stmts: vec![Stmt::Let {
                ident: "a".to_string(),
                expr: Expr::Ident("b".to_string()),
            }],
            errors: vec![],
        };

        assert_eq!(program.to_string(), "let a = b;".to_string());
    }
}
