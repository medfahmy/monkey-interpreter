mod ast;
mod lexer;
mod token;

pub use ast::{Expr, Program, Stmt};
pub use lexer::Lexer;
pub use token::Token;

#[derive(Debug)]
pub struct Parser {
    lexer: Lexer,
    program: Program,
    curr_token: Token,
    peek_token: Token,
}

impl Parser {
    pub fn parse(input: &str) -> Program {
        let lexer = Lexer::new(input);
        let mut parser = Parser {
            lexer,
            curr_token: Token::Eof,
            peek_token: Token::Eof,
            program: Program::new(),
        };

        parser.next_token();
        parser.next_token();

        while parser.curr_token != Token::Eof {
            if let Some(stmt) = parser.parse_stmt() {
                parser.program.push_stmt(stmt);
            }

            parser.next_token();
        }

        parser.program
    }

    fn next_token(&mut self) {
        self.curr_token = self.peek_token.clone();
        self.peek_token = self.lexer.next();
    }

    fn parse_stmt(&mut self) -> Option<Stmt> {
        match self.curr_token.clone() {
            Token::Let => self.parse_let_stmt(),
            Token::Return => self.parse_ret_stmt(),
            Token::Ident(ident) => {
                if let Token::Assign(op) = self.peek_token {
                    self.next_token();
                    self.next_token();
                    let expr = self.parse_expr(0)?;
                    self.next_token();

                    Some(Stmt::Assign { op, ident, expr })
                } else {
                    self.parse_expr_stmt()
                }
            }
            _ => self.parse_expr_stmt(),
        }
    }

    fn parse_let_stmt(&mut self) -> Option<Stmt> {
        if let Token::Ident(name) = self.peek_token.clone() {
            self.next_token();

            if let Token::Assign('\0') = self.peek_token {
                self.next_token();
                self.next_token();

                if self.curr_token == Token::Semicolon {
                    self.program
                        .push_error("missing right value in let statement".to_string());
                    return None;
                }

                let stmt = self.parse_expr_stmt();

                if let Some(Stmt::Expr(value)) = stmt.as_ref() {
                    Some(Stmt::Let {
                        ident: name,
                        expr: value.clone(),
                    })
                } else {
                    self.program.push_error(format!(
                        "expected expression, found {:?}",
                        self.curr_token
                    ));
                    None
                }
            } else {
                self.program.push_error(format!(
                    "expected Assign, found {:?}",
                    self.peek_token
                ));

                while self.curr_token != Token::Semicolon {
                    self.next_token();
                }

                None
            }
        } else {
            self.program
                .push_error(format!("expected Ident, found {:?}", self.peek_token));
            None
        }
    }

    fn parse_ret_stmt(&mut self) -> Option<Stmt> {
        self.next_token();

        if let Token::Semicolon = self.curr_token {
            self.program.push_error("missing return value".to_string());
            return None;
        }

        let stmt = self.parse_expr_stmt()?;

        if let Stmt::Expr(expr) = stmt {
            Some(Stmt::Ret(expr))
        } else {
            self.program.push_error(format!(
                "expected expression, found {}",
                stmt.to_string()
            ));
            None
        }
    }

    fn parse_expr_stmt(&mut self) -> Option<Stmt> {
        let stmt = Stmt::Expr(self.parse_expr(0)?);

        if let Token::Semicolon = self.peek_token {
            self.next_token();
        }

        Some(stmt)
    }

    fn parse_expr(&mut self, prec: usize) -> Option<Expr> {
        if let Some(mut left) = self.prefix_parse() {
            while self.peek_token != Token::Semicolon && prec < self.peek_prec() {
                self.next_token();

                if let Some(right) = self.infix_parse(&left) {
                    left = right;
                } else {
                    return Some(left);
                }
            }

            Some(left)
        } else {
            self.no_prefix_error();
            None
        }
    }

    fn prefix_parse(&mut self) -> Option<Expr> {
        match self.curr_token.clone() {
            Token::Ident(ident) => {
                if self.peek_token == Token::Lparen {
                    self.next_token();

                    let args = self.parse_args();
                    self.next_token();

                    Some(Expr::FnCall { ident, args })
                } else {
                    Some(Expr::Ident(ident.clone()))
                }
            }
            Token::Lparen => self.parse_grouped_expr(),
            Token::Int(n) => {
                let res = n.parse();
                match res {
                    Ok(i) => Some(Expr::Int(i)),
                    Err(_) => {
                        self.program
                            .push_error(format!("could not parse {} as int", n));
                        None
                    }
                }
            }
            Token::True => Some(Expr::Bool(true)),
            Token::False => Some(Expr::Bool(false)),
            Token::Bang | Token::Sub => {
                let op = self.curr_token.clone();
                self.next_token();
                let value = self.parse_expr(5)?;

                Some(Expr::Prefix {
                    op,
                    expr: Box::new(value),
                })
            }
            Token::If => {
                self.next_token();
                let cond = self.parse_expr(0)?;
                self.next_token();

                if let Token::Lbrace = self.curr_token {
                    self.next_token();
                    let csq = self.parse_block_stmt();
                    self.next_token();

                    let mut alt = Vec::new();

                    if let Token::Else = self.curr_token {
                        self.next_token();

                        if let Token::Lbrace = self.curr_token {
                            self.next_token();
                            alt = self.parse_block_stmt();
                        }
                    }

                    Some(Expr::If {
                        cond: Box::new(cond),
                        csq,
                        alt,
                    })
                } else {
                    self.no_prefix_error();
                    None
                }
            }
            Token::Fn => {
                self.next_token();

                if let Token::Lparen = self.curr_token {
                    let params = self.parse_params();

                    if let Token::Lbrace = self.curr_token {
                        self.next_token();
                        let body = self.parse_block_stmt();
                        self.next_token();

                        return Some(Expr::Fn { params, body });
                    } else {
                        self.program
                            .push_error(format!("expected Lbrace, found {:?}", self.curr_token));
                        return None;
                    }
                } else {
                    self.program
                        .push_error(format!("expected Lparen, found {:?}", self.curr_token));
                    return None;
                }
            }
            _ => None,
        }
    }

    fn parse_block_stmt(&mut self) -> Vec<Stmt> {
        let mut stmts = Vec::new();
        while self.curr_token != Token::Rbrace && self.curr_token != Token::Eof {
            if let Some(stmt) = self.parse_stmt() {
                stmts.push(stmt);
            }

            self.next_token();
        }

        stmts
    }

    fn parse_params(&mut self) -> Vec<String> {
        let mut exprs = Vec::new();
        self.next_token();

        while self.curr_token != Token::Rparen {
            if let Some(Expr::Ident(ident)) = self.parse_expr(0) {
                exprs.push(ident);
                self.next_token();

                if let Token::Comma = self.curr_token {
                    self.next_token();
                    continue;
                }
            } else {
                self.program
                    .push_error(format!("expected Ident, found {:?}", self.curr_token));
                return exprs;
            }
        }

        self.next_token();
        exprs
    }

    fn parse_args(&mut self) -> Vec<Expr> {
        let mut exprs = Vec::new();
        self.next_token();

        while self.curr_token != Token::Rparen {
            if let Some(expr) = self.parse_expr(0) {
                exprs.push(expr);
                self.next_token();

                if let Token::Rparen = self.curr_token {
                    self.next_token();
                    return exprs;
                } else if let Token::Comma = self.curr_token {
                    self.next_token();
                } else {
                    self.program
                        .push_error(format!("expected Comma, found {:?}", self.curr_token));
                    return exprs;
                }
            }
        }

        exprs
    }

    fn infix_parse(&mut self, left: &Expr) -> Option<Expr> {
        match &self.curr_token {
            Token::Mul
            | Token::Div
            | Token::Add
            | Token::Sub
            | Token::Eq
            | Token::NotEq
            | Token::Lt
            | Token::Gt => {
                let op = self.curr_token.clone();
                let prec = self.curr_prec();
                self.next_token();
                let right = self.parse_expr(prec)?;
                Some(Expr::Infix {
                    op,
                    left: Box::new(left.clone()),
                    right: Box::new(right),
                })
            }
            _ => None,
        }
    }

    fn parse_grouped_expr(&mut self) -> Option<Expr> {
        self.next_token();
        let expr = self.parse_expr(0);

        if self.peek_token != Token::Rparen {
            self.next_token();
            return None;
        }

        self.next_token();
        expr
    }

    fn no_prefix_error(&mut self) {
        let error = format!("unable to parse prefix {:?}", self.curr_token);
        self.program.push_error(error);
    }

    fn curr_prec(&self) -> usize {
        Parser::check_prec(&self.curr_token)
    }

    fn peek_prec(&self) -> usize {
        Parser::check_prec(&self.peek_token)
    }

    fn check_prec(token: &Token) -> usize {
        match token {
            Token::Eq => 1,
            Token::NotEq => 1,
            Token::Lt => 2,
            Token::Gt => 2,
            Token::Add => 3,
            Token::Sub => 3,
            Token::Mul => 4,
            Token::Div => 4,
            _ => 0,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_test(input: &str, exp_stmts_nr: usize, exp_errors_nr: usize) -> Vec<Stmt> {
        let program = Parser::parse(input);
        let stmts = program.stmts();
        let errors = program.errors();

        assert_eq!(
            stmts.len(),
            exp_stmts_nr,
            "input: {}, num of stmts, stmts = {:?}",
            input,
            stmts
        );
        assert_eq!(
            errors.len(),
            exp_errors_nr,
            "input: {}, num of errors, errors = {:?}",
            input,
            errors
        );

        stmts.clone()
    }

    fn let_test(stmt: &Stmt, exp_ident: String, exp_value: String) {
        if let Stmt::Let { ident, expr: value } = stmt {
            assert_eq!(ident.to_string(), exp_ident);
            assert_eq!(value.to_string(), exp_value);
        } else {
            panic!("stmt is not let: {:?}", &stmt);
        }
    }

    #[test]
    // #[ignore]
    fn let_stmt() {
        let input = r#"let x = 69;
            let y = 69 + 420;
            let foo = bar(x);"#;

        let stmts = parse_test(input, 3, 0);

        let names = ["x", "y", "foo"];
        let values = ["69", "(69 + 420)", "bar(x)"];

        for i in 0..3 {
            // let name = names[i];
            // let stmt = stmts[i].clone();
            let_test(
                &stmts[i].clone(),
                names[i].to_string(),
                values[i].to_string(),
            );
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
    // #[ignore]
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

        if let Stmt::Expr(Expr::Prefix { op, expr }) = stmt {
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
        prefix_expr_test("-15", Token::Sub, 15);
    }

    #[test]
    fn infix_expr() {
        let inputs = [
            "5 + 5;", "5 - 5;", "5 * 5;", "5 / 5;", "5 < 5;", "5 > 5;", "5 == 5;", "5 != 5;",
        ];
        let ops = ["+", "-", "*", "/", "<", ">", "==", "!="];

        for i in 0..inputs.len() {
            let stmt = parse_test(inputs[i], 1, 0)[0].clone();

            if let Stmt::Expr(expr) = stmt {
                if let Expr::Infix { op, left, right } = expr {
                    assert_eq!(op.to_string(), ops[i].to_string());

                    match (*left.clone(), *right.clone()) {
                        (Expr::Int(5), Expr::Int(5)) => {}
                        _ => panic!(
                            "incorrect operands: {}, {}",
                            left.to_string(),
                            right.to_string()
                        ),
                    }
                }
            } else {
                panic!("stmt is not expr: {:?}", stmt);
            }
        }
    }

    #[test]
    fn infix_many() {
        let tests = [
            ["-a * b", "((-a) * b)"],
            ["!-a", "(!(-a))"],
            ["a + b + c", "((a + b) + c)"],
            ["a + b - c", "((a + b) - c)"],
            ["a * b * c", "((a * b) * c)"],
            ["a * b / c", "((a * b) / c)"],
            ["a + b / c", "(a + (b / c))"],
            ["a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"],
            ["5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"],
            ["5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"],
            [
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ],
            [
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ],
        ];

        for test in tests {
            let stmts = parse_test(test[0], 1, 0);
            assert_eq!(stmts[0].to_string(), test[1].to_string());
        }
    }

    #[test]
    fn if_expr() {
        let input = "if (x < y) { x }";
        let stmts = parse_test(input, 1, 0);

        if let Stmt::Expr(Expr::If { cond, csq, alt }) = stmts[0].clone() {
            assert_eq!(cond.to_string(), "(x < y)");
            assert_eq!(csq.len(), 1);
            assert_eq!(csq[0].to_string(), "x");
            assert!(alt.is_empty());
        } else {
            panic!("expected if expr, found {:?}", stmts[0]);
        }
    }

    #[test]
    fn if_else() {
        let input = "if (x < y) { x } else { y }";
        let stmts = parse_test(input, 1, 0);

        if let Stmt::Expr(Expr::If { cond, csq, alt }) = stmts[0].clone() {
            assert_eq!(cond.to_string(), "(x < y)");
            assert_eq!(csq.len(), 1);
            assert_eq!(csq[0].to_string(), "x");
            assert_eq!(alt.len(), 1);
            assert_eq!(alt[0].to_string(), "y");
        } else {
            panic!("expected if expr, found {:?}", stmts[0]);
        }
    }

    #[test]
    fn func() {
        let input = "fn(x, y) { return x + y }";
        let stmts = parse_test(input, 1, 0);

        if let Stmt::Expr(Expr::Fn { params, body }) = stmts[0].clone() {
            assert_eq!(params.len(), 2);
            assert_eq!(params[0].to_string(), "x");
            assert_eq!(params[1].to_string(), "y");

            assert_eq!(body.len(), 1);
            assert_eq!(body[0].to_string(), "return (x + y);");
        } else {
            panic!("expected fn expr, found {:?}", stmts[0]);
        }
    }

    #[test]
    fn infix_fn_call() {
        let input = "add(1, 2)";
        let stmts = parse_test(input, 1, 0);

        if let Stmt::Expr(Expr::FnCall { ident, args }) = stmts[0].clone() {
            assert_eq!(ident, "add");
            assert_eq!(args.len(), 2);
            assert_eq!(args[0].to_string(), "1");
            assert_eq!(args[1].to_string(), "2");
        } else {
            panic!("expected call expr, found {:?}", stmts[0]);
        }
    }
}
