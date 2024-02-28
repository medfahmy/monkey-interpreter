mod ast;
mod lexer;
mod token;

use ast::{Expr, Program, Stmt};
use lexer::Lexer;
use token::Token;

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
                self.next_token();
                self.next_token();


                if self.curr_token == Token::Semicolon {
                    self.program.push_error("missing right value in let statement".to_string());
                    return None;
                }

                let stmt = self.parse_expr_stmt();

                if let Stmt::Expr(expr) = stmt.as_ref().unwrap() {
                    Some(Stmt::Let(Expr::Ident(name), expr.clone()))
                } else {
                    self.program.push_error(format!(
                        "expected expression, got {} instead",
                        stmt.unwrap().to_string()
                    ));
                    None
                }
            } else {
                self.program.push_error(format!(
                    "expected Assign, got {:?} instead",
                    self.peek_token
                ));

                while self.curr_token != Token::Semicolon {
                    self.next_token();
                }

                None
            }
        } else {
            self.program.push_error(
                format!("expected Ident, got {:?} instead", 
                self.peek_token)
            );
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
                "expected expression, got {} instead",
                stmt.to_string()
            ));
            None
        }
    }

    fn parse_expr_stmt(&mut self) -> Option<Stmt> {
        let stmt = Stmt::Expr(self.parse_expr(Prec::Low)?);

        if let Token::Semicolon = self.peek_token {
            self.next_token();
        }

        Some(stmt)
    }

    fn parse_expr(&mut self, prec: Prec) -> Option<Expr> {
        // println!("begin {:?}", self.curr_token);

        if let Some(mut left) = self.prefix_parse() {
            while self.peek_token != Token::Semicolon
                && (prec as usize) < (self.peek_prec() as usize)
            {
                self.next_token();

                if let Some(right) = self.infix_parse(&left) {
                    left = right;
                } else {
                    return Some(left);
                }
            }

            // println!("parsed {:?}", left);
            // println!("end {:?}", self.curr_token);
            Some(left)
        } else {
            self.no_prefix_error();
            None
        }
    }

    fn prefix_parse(&mut self) -> Option<Expr> {
        match &self.curr_token {
            Token::Lparen => self.parse_grouped_expr(),
            Token::Ident(ident) => Some(Expr::Ident(ident.clone())),
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
            Token::Bang | Token::Minus => {
                let op = self.curr_token.clone();
                self.next_token();
                let expr = self.parse_expr(Prec::Prefix)?;
                Some(Expr::Prefix(op, Box::new(expr)))
            }
            Token::If => {
                self.next_token();
                let cond = self.parse_expr(Prec::Low)?;
                self.next_token();

                if let Token::Lbrace = self.curr_token {
                    self.next_token();
                    let csq = self.parse_block_stmt();
                    self.next_token();

                    let mut alt = None;

                    if let Token::Else = self.curr_token {
                        self.next_token();

                        if let Token::Lbrace = self.curr_token {
                            self.next_token();
                            alt = Some(self.parse_block_stmt());
                        }
                    }

                    Some(Expr::If(Box::new(cond), csq, alt))
                } else {
                    self.no_prefix_error();
                    None
                }
            }
            Token::Fn => {
                self.next_token();
                if let Token::Lparen = self.curr_token {
                    self.next_token();
                    let args = self.parse_fn_args()?;
                    
                    if let Token::Lbrace = self.curr_token {
                        self.next_token();
                        let stmts = self.parse_block_stmt();

                        return Some(Expr::Fn(args, stmts));
                    } else {
                        self.program.push_error(format!("expected Rparen, found {:?}", self.curr_token));
                        return None;
                    }
                } else {
                    self.program.push_error(format!("expected Lbrace, found {:?}", self.curr_token));
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

    fn parse_fn_args(&mut self) -> Option<Vec<Expr>> {
        let mut exprs = Vec::new();

        while self.curr_token != Token::Eof {
            if let Some(Expr::Ident(ident)) = self.parse_expr(Prec::Low) {
                exprs.push(Expr::Ident(ident));
                self.next_token();

                if let Token::Rparen = self.curr_token {
                    self.next_token();
                    return Some(exprs);
                } else if let Token::Comma = self.curr_token {
                    self.next_token();
                } else {
                    self.program.push_error(format!("expected Comma, found {:?}", self.curr_token));
                    return None;
                }
            } else {
                self.program.push_error(format!("expected Ident, found {:?}", self.curr_token));
                return None;
            }
        }

        Some(exprs)
    }

    fn infix_parse(&mut self, left: &Expr) -> Option<Expr> {
        match &self.curr_token {
            Token::Asterisk
            | Token::Slash
            | Token::Plus
            | Token::Minus
            | Token::Eq
            | Token::NotEq
            | Token::Lt
            | Token::Gt => {
                let token = self.curr_token.clone();
                let prec = self.curr_prec();
                self.next_token();
                let right = self.parse_expr(prec)?;
                Some(Expr::Infix(token, Box::new(left.clone()), Box::new(right)))
            }
            _ => None,
        }
    }

    fn parse_grouped_expr(&mut self) -> Option<Expr> {
        // println!("begin grouped {:?}", self.curr_token);
        self.next_token();
        let expr = self.parse_expr(Prec::Low);

        if self.peek_token != Token::Rparen {
            self.next_token();
            return None;
        }

        self.next_token();
        // println!("end grouped {:?}", self.curr_token);
        expr
    }

    fn no_prefix_error(&mut self) {
        let error = format!("unable to parse prefix {:?}", self.curr_token);
        self.program.push_error(error);
    }

    fn peek_prec(&self) -> Prec {
        Prec::from(&self.peek_token)
    }

    fn curr_prec(&self) -> Prec {
        Prec::from(&self.curr_token)
    }
}

#[derive(Clone, Copy, Debug)]
enum Prec {
    Low = 0,
    Eq = 1,     // ==
    Lg = 2,     // <, >
    Sum = 3,    // +
    Prod = 4,   // *
    Prefix = 5, // -x, !x
    // Call = 6,   // foo(x)
}

impl From<&Token> for Prec {
    fn from(token: &Token) -> Self {
        match token {
            Token::Eq => Self::Eq,
            Token::NotEq => Self::Eq,
            Token::Lt => Self::Lg,
            Token::Gt => Self::Lg,
            Token::Plus => Self::Sum,
            Token::Minus => Self::Sum,
            Token::Slash => Self::Prod,
            Token::Asterisk => Self::Prod,
            _ => Self::Low,
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

    fn let_test(stmt: &Stmt, name: String) {
        if let Stmt::Let(ident, _) = stmt {
            assert_eq!(ident, &Expr::Ident(name.clone()));
        } else {
            panic!("stmt is not let: {:?}", &stmt);
        }
    }

    #[test]
    #[ignore]
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
    #[ignore]
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

    #[test]
    fn infix_expr() {
        let inputs = [
            "5 + 5;", "5 - 5;", "5 * 5;", "5 / 5;", "5 < 5;", "5 > 5;", "5 == 5;", "5 != 5;",
        ];
        let ops = ["+", "-", "*", "/", "<", ">", "==", "!="];

        for i in 0..inputs.len() {
            let stmt = parse_test(inputs[i], 1, 0)[0].clone();

            if let Stmt::Expr(expr) = stmt {
                if let Expr::Infix(token, l, r) = expr {
                    assert_eq!(token.to_string(), ops[i].to_string());

                    match (*l.clone(), *r.clone()) {
                        (Expr::Int(5), Expr::Int(5)) => {}
                        _ => panic!("incorrect operands: {}, {}", l.to_string(), r.to_string()),
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
    #[ignore]
    fn infix_fn_call() {
        let input = "add(1, 2)";
        let stmts = parse_test(input, 1, 0);

        if let Stmt::Expr(Expr::Call(fn_name, args)) = stmts[0].clone() {
            assert_eq!(fn_name, "add".to_string());

            match args[0] {
                Expr::Int(1) => {},
                _ => panic!("incorrect expr: {}", args[0].to_string()),
            }

            match args[1] {
                Expr::Int(2) => {},
                _ => panic!("incorrect expr: {}", args[1].to_string()),
            }
        }
    }

    #[test]
    fn if_expr() {
        let input = "if (x < y) { x }";
        let stmts = parse_test(input, 1, 0);

        if let Stmt::Expr(Expr::If(cond, csq, alt)) = stmts[0].clone() {
            assert_eq!(cond.to_string(), "(x < y)");
            assert_eq!(csq.len(), 1);
            assert_eq!(csq[0].to_string(), "x");
            assert_eq!(alt, None);
        }
    }

    #[test]
    fn if_else() {
        let input = "if (x < y) { x } else { y }";
        let stmts = parse_test(input, 1, 0);

        if let Stmt::Expr(Expr::If(cond, csq, alt)) = stmts[0].clone() {
            assert_eq!(cond.to_string(), "(x < y)");
            assert_eq!(csq.len(), 1);
            assert_eq!(csq[0].to_string(), "x");
            let alt = alt.unwrap();
            assert_eq!(alt.len(), 1);
            assert_eq!(alt[0].to_string(), "y");
        }
    }

    #[test]
    fn func() {
        let input = "fn(x < y) { return x + y }";
        let stmts = parse_test(input, 1, 0);

        if let Stmt::Expr(Expr::Fn(args, stmts)) = stmts[0].clone() {
            assert_eq!(args.len(), 2);
            assert_eq!(args[0].to_string(), "x");
            assert_eq!(args[1].to_string(), "y");

            assert_eq!(stmts.len(), 1);
            assert_eq!(stmts[0].to_string(), "return x + y;");
        }
    }
}
