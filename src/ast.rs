#[derive(Debug)]
pub struct Program {
    pub stmts: Vec<Stmt>,
}

impl ToString for Program {
    fn to_string(&self) -> String {
        self.stmts
            .iter()
            .map(|stmt| stmt.to_string())
            .collect::<Vec<_>>()
            .join("")
    }
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Let(Expr, Expr),
    Ret(Expr),
    Expr(Expr),
}

impl ToString for Stmt {
    fn to_string(&self) -> String {
        match self {
            Self::Let(name, value) => {
                format!("let {} = {};", name.to_string(), value.to_string())
            },
            Self::Ret(value) => {
                format!("return {};", value.to_string())
            },
            Self::Expr(expr) => {
                expr.to_string()
            },
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Ident(String),
    Value,
}

impl ToString for Expr {
    fn to_string(&self) -> String {
        match self {
            Expr::Ident(name) => name.to_string(),
            Expr::Value => unimplemented!(),
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
        };

        assert_eq!(program.to_string(), "let a = b;".to_string());
    }
}
