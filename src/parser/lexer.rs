use super::token::Token;

#[derive(Debug)]
pub struct Lexer {
    input: String,
    curr_pos: usize,
    peek_pos: usize,
    curr: char,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let mut lexer = Self {
            input: input.to_string(),
            curr_pos: 0,
            peek_pos: 0,
            curr: '\0',
        };

        lexer.read_char();
        lexer
    }

    pub fn next(&mut self) -> Token {
        self.skip_whitespace();

        use Token::*;

        let token = match self.curr {
            '\0' => Eof,
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Eq
                } else {
                    Assign('\0')
                }
            }
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    NotEq
                } else {
                    Bang
                }
            }
            // '+' => Plus,
            '+' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Assign('+')
                } else {
                    Add
                }
            }
            '-' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Assign('-')
                } else {
                    Sub
                }
            }
            '*' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Assign('*')
                } else {
                    Mul
                }
            }
            '/' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Assign('/')
                } else {
                    Div
                }
            }
            '(' => Lparen,
            ')' => Rparen,
            '{' => Lbrace,
            '}' => Rbrace,
            ';' => Semicolon,
            ',' => Comma,
            '<' => Lt,
            '>' => Gt,
            _ => {
                if self.curr.is_alphabetic() || self.curr == '_' {
                    return self.read_ident();
                } else if self.curr.is_ascii_digit() {
                    return self.read_number();
                } else {
                    Illegal
                }
            }
        };

        self.read_char();
        token
    }

    fn read_char(&mut self) {
        if self.peek_pos >= self.input.len() {
            self.curr = '\0';
        } else {
            // todo: handle error
            self.curr = self.input.chars().nth(self.peek_pos).unwrap();
        }

        self.curr_pos = self.peek_pos;
        self.peek_pos += 1;
    }

    fn read_ident(&mut self) -> Token {
        let position = self.curr_pos;

        while self.curr.is_alphabetic() || self.curr == '_' {
            self.read_char();
        }

        let s = &self.input[position..self.curr_pos];

        use Token::*;

        match s {
            "let" => Let,
            "fn" => Fn,
            "if" => If,
            "else" => Else,
            "return" => Return,
            "true" => True,
            "false" => False,
            s => Ident(s.to_string()),
        }
    }

    fn read_number(&mut self) -> Token {
        let position = self.curr_pos;

        while self.curr.is_ascii_digit() {
            self.read_char();
        }

        let num = &self.input[position..self.curr_pos];

        Token::Int(num.to_string())
    }

    fn skip_whitespace(&mut self) {
        while self.curr.is_whitespace() {
            self.read_char();
        }
    }

    fn peek_char(&self) -> char {
        if self.peek_pos >= self.input.len() {
            '\0'
        } else {
            self.input.chars().nth(self.peek_pos).unwrap()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use Token::*;

    fn lex_test(input: &str, tokens: Vec<Token>) {
        let mut lexer = Lexer::new(input);

        for expected_token in tokens {
            let token = lexer.next();
            assert_eq!(token, expected_token);
        }
    }

    #[test]
    fn symbols() {
        let input = "=+(){},;";
        let tokens = vec![
            Assign('\0'),
            Add,
            Lparen,
            Rparen,
            Lbrace,
            Rbrace,
            Comma,
            Semicolon,
            Eof,
        ];

        lex_test(input, tokens);
    }

    #[test]
    fn ident_num() {
        let input = "let five = 5; let ten = 10;";
        let tokens = vec![
            Let,
            Ident("five".to_string()),
            Assign('\0'),
            Int("5".to_string()),
            Semicolon,
            Let,
            Ident("ten".to_string()),
            Assign('\0'),
            Int("10".to_string()),
            Semicolon,
            Eof,
        ];

        lex_test(input, tokens);
    }

    #[test]
    fn function() {
        let input = r#"let add = fn(x, y) {
                x + y;
            };
            let result = add(five, ten);"#;

        let tokens = vec![
            Let,
            Ident("add".to_string()),
            Assign('\0'),
            Fn,
            Lparen,
            Ident("x".to_string()),
            Comma,
            Ident("y".to_string()),
            Rparen,
            Lbrace,
            Ident("x".to_string()),
            Add,
            Ident("y".to_string()),
            Semicolon,
            Rbrace,
            Semicolon,
            Let,
            Ident("result".to_string()),
            Assign('\0'),
            Ident("add".to_string()),
            Lparen,
            Ident("five".to_string()),
            Comma,
            Ident("ten".to_string()),
            Rparen,
            Semicolon,
            Eof,
        ];

        lex_test(input, tokens);
    }

    #[test]
    fn operators() {
        let input = r#"!-/*5;
        5 < 10 > 5;
        10 == 10;
        10 != 9;"#;

        let tokens = vec![
            Bang,
            Sub,
            Div,
            Mul,
            Int("5".to_string()),
            Semicolon,
            Int("5".to_string()),
            Lt,
            Int("10".to_string()),
            Gt,
            Int("5".to_string()),
            Semicolon,
            Int("10".to_string()),
            Eq,
            Int("10".to_string()),
            Semicolon,
            Int("10".to_string()),
            NotEq,
            Int("9".to_string()),
            Semicolon,
            Eof,
        ];

        lex_test(input, tokens);
    }

    #[test]
    fn keywords() {
        let input = r#"if (5 < 10) {
            return true;
        } else {
            return false;
        }"#;
        let tokens = vec![
            If,
            Lparen,
            Int("5".to_string()),
            Lt,
            Int("10".to_string()),
            Rparen,
            Lbrace,
            Return,
            True,
            Semicolon,
            Rbrace,
            Else,
            Lbrace,
            Return,
            False,
            Semicolon,
            Rbrace,
            Eof,
        ];

        lex_test(input, tokens);
    }
}
