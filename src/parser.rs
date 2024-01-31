use crate::ast::{Expr, Stmt};
use crate::{Lexer, Program, Token};

#[derive(Debug)]
pub struct Parser {
    lexer: Lexer,
    curr_token: Token,
    peek_token: Token,
    errs: Vec<String>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
        let mut parser = Parser {
            lexer,
            curr_token: Token::Eof,
            peek_token: Token::Eof,
            errs: Vec::new(),
        };

        parser.next_token();
        parser.next_token();

        parser
    }

    fn next_token(&mut self) {
        self.curr_token = self.peek_token.clone();
        self.peek_token = self.lexer.next();
    }

    pub fn parse(&mut self) -> Program {
        let mut program = Program { stmts: Vec::new() };

        while self.curr_token != Token::Eof {
            if let Some(stmt) = self.parse_stmt() {
                program.stmts.push(stmt);
            }

            self.next_token();
        }

        program
    }

    pub fn errors(&self) -> &Vec<String> {
        &self.errs
    }

    fn parse_stmt(&mut self) -> Option<Stmt> {
        match self.curr_token {
            Token::Let => self.parse_let_stmt(),
            Token::Return => self.parse_ret_stmt(),
            _ => None,
        }
    }

    fn parse_let_stmt(&mut self) -> Option<Stmt> {
        if let Token::Ident(name) = self.peek_token.clone() {
            self.next_token();

            if let Token::Assign = self.peek_token {
                while self.curr_token != Token::Semicolon {
                    self.next_token();
                }

                Some(Stmt::Let(Expr::Ident(name.clone()), Expr::Value))
            } else {
                self.errs.push(format!(
                    "expected Assign, got {:?} instead",
                    self.peek_token
                ));

                None
            }
        } else {
            self.errs
                .push(format!("expected Ident, got {:?} instead", self.peek_token));

            return None;
        }
    }

    fn parse_ret_stmt(&mut self) -> Option<Stmt> {
        self.next_token();

        while self.curr_token != Token::Semicolon {
            self.next_token();
        }

        Some(Stmt::Ret(Expr::Value))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        ast::{Expr, Stmt},
        Lexer,
    };

    fn let_test(stmt: &Stmt, name: String) {
        if let Stmt::Let(ident, value) = stmt {
            assert_eq!(ident, &Expr::Ident(name.clone()));
            assert_eq!(value, &Expr::Value);
        } else {
            panic!("stmt is not let: {:?}", &stmt);
        }
    }

    #[test]
    fn let_works() {
        let input = r#"let x = 69;
            let y = 69 + 420;
            let foo = bar(x);"#;

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let program = parser.parse();

        assert_eq!(program.stmts.len(), 3);

        let names = ["x", "y", "foo"];

        for i in 0..3 {
            let name = names[i];
            let stmt = &program.stmts[i];
            let_test(stmt, name.to_string());
        }
    }

    #[test]
    fn let_errors_works() {
        let input = r#"let x 69;
            let = 420;
            let 191919;"#;
        
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let program = parser.parse();

        assert_eq!(parser.errs.len(), 3);
        assert_eq!(program.stmts.len(), 0);
    }

    #[test]
    fn return_works() {
        let input = r#"return 69;
            return 5 + 10;
            return add(x + y);"#;

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let program = parser.parse();

        assert_eq!(program.stmts.len(), 3);

        for stmt in program.stmts {
            assert!(matches!(stmt, Stmt::Ret(..)));
        }
    }
}
