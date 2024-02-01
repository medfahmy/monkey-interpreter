use crate::ast::{Expr, Stmt};
use crate::{Lexer, Program, Token};

#[derive(Debug)]
pub struct Parser {
    lexer: Lexer,
    curr_token: Token,
    peek_token: Token,
    errors: Vec<String>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
        let mut parser = Parser {
            lexer,
            curr_token: Token::Eof,
            peek_token: Token::Eof,
            errors: Vec::new(),
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
        let mut program = Program::new();

        while self.curr_token != Token::Eof {
            if let Some(stmt) = self.parse_stmt() {
                program.push_stmt(stmt);
            }

            self.next_token();
        }

        program
    }

    pub fn errors(&self) -> &Vec<String> {
        &self.errors
    }

    fn parse_stmt(&mut self) -> Option<Stmt> {
        match self.curr_token {
            Token::Let => self.parse_let_stmt(),
            Token::Return => self.parse_ret_stmt(),
            _ => self.parse_expr_stmt(),
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
                self.errors.push(format!(
                    "expected Assign, got {:?} instead",
                    self.peek_token
                ));

                None
            }
        } else {
            self.errors
                .push(format!("expected Ident, got {:?} instead", self.peek_token));

            None
        }
    }

    fn parse_ret_stmt(&mut self) -> Option<Stmt> {
        self.next_token();

        while self.curr_token != Token::Semicolon {
            self.next_token();
        }

        Some(Stmt::Ret(Expr::Value))
    }

    fn parse_expr_stmt(&mut self) -> Option<Stmt> {
        let stmt = Stmt::Expr(self.parse_expr(Prec::Low)?);

        if let Token::Semicolon = self.peek_token {
            self.next_token();
        }

        Some(stmt)
    }

    fn parse_expr(&mut self, prec: Prec) -> Option<Expr> {
        match self.prefix_parse() {
            Some(expr) => Some(expr),
            None => {
                self.no_prefix_error();
                None
            }
        }
    }

    fn prefix_parse(&mut self) -> Option<Expr> {
        match &self.curr_token {
            Token::Ident(ident) => Some(Expr::Ident(ident.clone())),
            Token::Int(n) => {
                let res = n.parse();
                match res {
                    Ok(i) => Some(Expr::Int(i)),
                    Err(_) => {
                        self.errors.push(format!("could not parse {} as int", n));
                        None
                    }
                }
            }
            Token::Bang | Token::Minus => {
                let op = self.curr_token.clone();
                self.next_token();
                let expr = self.parse_expr(Prec::Prefix)?;
                Some(Expr::Prefix(op, Box::new(expr)))
            }
            _ => None,
        }
    }

    fn infix_parse(&self, token: &Token) -> Option<Expr> {
        todo!()
    }

    fn no_prefix_error(&mut self) {
        self.errors
            .push(format!("unable to parse prefix {:?}", self.curr_token));
    }
}

enum Prec {
    Low = 0,
    Eq = 1,   // ==
    Lg = 2,   // <, >
    Sum = 3,  // +
    Prod = 4, // *
    Prefix = 5,  // -x, !x
    Call = 6, // foo(x)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        ast::{Expr, Stmt},
        Lexer,
    };

    fn parse_test(input: &str, exp_stmts_nr: usize, exp_errors_nr: usize) -> Vec<Stmt> {
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        let stmts = program.stmts();
        let errors = parser.errors();

        assert_eq!(
            stmts.len(),
            exp_stmts_nr,
            "num of stmts, stmts = {:?}",
            stmts
        );
        assert_eq!(
            errors.len(),
            exp_errors_nr,
            "num of errors, errors = {:?}",
            errors
        );

        stmts.clone()
    }

    fn let_test(stmt: &Stmt, name: String) {
        if let Stmt::Let(ident, value) = stmt {
            assert_eq!(ident, &Expr::Ident(name.clone()));
            assert_eq!(value, &Expr::Value);
        } else {
            panic!("stmt is not let: {:?}", &stmt);
        }
    }

    #[test]
    fn let_stmt() {
        let input = r#"let x = 69;
            let y = 69 + 420;
            let foo = bar(x);"#;

        let stmts = parse_test(input, 3, 0);

        let names = ["x", "y", "foo"];

        for i in 0..3 {
            let name = names[i];
            let stmt = stmts[i].clone();
            let_test(&stmt, name.to_string());
        }
    }

    #[test]
    #[ignore]
    fn errors() {
        let input = r#"let x 69;
            let = 420;
            let 191919;"#;

        let _ = parse_test(input, 0, 3);
    }

    #[test]
    fn ret_stmt() {
        let input = r#"return 69;
            return 5 + 10;
            return add(x + y);"#;

        let stmts = parse_test(input, 3, 0);

        for stmt in stmts {
            assert!(matches!(stmt, Stmt::Ret(..)));
        }
    }

    #[test]
    fn ident_expr() {
        let input = "foobar;";
        let stmts = parse_test(input, 1, 0);

        let stmt = stmts[0].clone();
        assert!(matches!(stmt, Stmt::Expr(..)), "stmt is not expr");

        if let Stmt::Expr(Expr::Ident(ident)) = stmt {
            assert_eq!(ident, "foobar");
        } else {
            panic!("expr is not ident: {:?}", stmt);
        }
    }

    #[test]
    fn int_expr() {
        let input = "69;";
        let stmts = parse_test(input, 1, 0);
        let stmt = stmts[0].clone();

        assert!(matches!(stmt, Stmt::Expr(..)), "stmt is not expr");

        if let Stmt::Expr(Expr::Int(n)) = stmt {
            assert_eq!(n, 69);
        } else {
            panic!("expr is not int: {:?}", stmt);
        }
    }

    fn prefix_expr_test(input: &str, exp_op: Token, exp_n: i64) {
        let stmt = parse_test(input, 1, 0)[0].clone();

        assert!(matches!(stmt, Stmt::Expr(..)), "stmt is not expr");

        if let Stmt::Expr(Expr::Prefix(op, expr)) = stmt {
            assert_eq!(op, exp_op);
            let expr = *expr;
            assert!(matches!(expr, Expr::Int(..)), "epxr is not int");

            if let Expr::Int(n) = expr {
                assert_eq!(n, exp_n, "incorrect int inside expr");
            }
        } else {
            panic!("expr is not prefix: {:?}", stmt);
        }
    }

    #[test]
    fn prefix_expr() {
        prefix_expr_test("!5", Token::Bang, 5);
        prefix_expr_test("-15", Token::Minus, 15);
    }
}
