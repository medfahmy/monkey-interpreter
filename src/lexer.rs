use std::fmt::Display;

#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    // keywords
    Let,
    Fn,
    If,
    Else,
    Return,
    True,
    False,

    Ident(&'a str),

    // data types
    Int(i64),
    // Float(f64),

    // keywords
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Eq,
    NotEq,
    Lt,
    Gt,

    // symbols
    Semicolon,
    Comma,
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,

    Illegal,
    Eof,
}

#[derive(Debug)]
pub struct Lexer<'a> {
    input: &'a str,
    position: usize,
    read_position: usize,
    curr: char,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Self {
            input,
            position: 0,
            read_position: 0,
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
                    Assign
                }
            },
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    NotEq
                } else {
                    Bang
                }
            },
            '+' => Plus,
            '(' => Lparen,
            ')' => Rparen,
            '{' => Lbrace,
            '}' => Rbrace,
            ';' => Semicolon,
            ',' => Comma,
            '-' => Minus,
            '*' => Asterisk,
            '/' => Slash,
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
        if self.read_position >= self.input.len() {
            self.curr = '\0';
        } else {
            // todo: handle error
            self.curr = self.input.chars().nth(self.read_position).unwrap();
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn read_ident(&mut self) -> Token {
        let position = self.position;

        while self.curr.is_alphabetic() || self.curr == '_' {
            self.read_char();
        }

        let s = &self.input[position..self.position];

        use Token::*;

        match s {
            "let" => Let,
            "fn" => Fn,
            "if" => If,
            "else" => Else,
            "return" => Return,
            "true" => True,
            "false" => False,
            s => Ident(s),
        }
    }

    fn read_number(&mut self) -> Token {
        let position = self.position;

        while self.curr.is_ascii_digit() {
            self.read_char();
        }

        let num = &self.input[position..self.position];

        Token::Int(num.parse().unwrap())
    }

    fn skip_whitespace(&mut self) {
        while self.curr.is_whitespace() {
            self.read_char();
        }
    }

    fn peek_char(&self) -> char {
        if self.read_position >= self.input.len() {
            '\0'
        } else {
            self.input.chars().nth(self.read_position).unwrap()
        }
    }
}

// impl<'a> Iterator for Lexer<'a> {
//     type Item = Token<'a>;
//
//     fn next(&mut self) -> Option<Self::Item> {
//         self.skip_whitespace();
//
//         if self.curr == '\0' {
//             return None;
//         }
//
//         use Token::*;
//         let token = match self.curr {
//             '=' => Assign,
//             '+' => Plus,
//             '(' => Lparen,
//             ')' => Rparen,
//             '{' => Lbrace,
//             '}' => Rbrace,
//             ';' => Semicolon,
//             ',' => Comma,
//             '-' => Minus,
//             '!' => Bang,
//             '*' => Asterisk,
//             '/' => Slash,
//             '<' => Lt,
//             '>' => Gt,
//             _ => {
//
//                 if self.curr.is_alphabetic() || self.curr == '_' {
//                     return Some(self.read_ident());
//                 } else if self.curr.is_ascii_digit() {
//                     return Some(self.read_number());
//                 } else {
//                     Illegal
//                 }
//             },
//         };
//
//         self.read_char();
//         Some(token)
//     }
// }

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
            Assign, Plus, Lparen, Rparen, Lbrace, Rbrace, Comma, Semicolon, Eof,
        ];

        lex_test(input, tokens);
    }

    #[test]
    fn ident_num() {
        let input = "let five = 5; let ten = 10;";
        let tokens = vec![
            Let,
            Ident("five"),
            Assign,
            Int(5),
            Semicolon,
            Let,
            Ident("ten"),
            Assign,
            Int(10),
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
            Ident("add"),
            Assign,
            Fn,
            Lparen,
            Ident("x"),
            Comma,
            Ident("y"),
            Rparen,
            Lbrace,
            Ident("x"),
            Plus,
            Ident("y"),
            Semicolon,
            Rbrace,
            Semicolon,
            Let,
            Ident("result"),
            Assign,
            Ident("add"),
            Lparen,
            Ident("five"),
            Comma,
            Ident("ten"),
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
            Minus,
            Slash,
            Asterisk,
            Int(5),
            Semicolon,
            Int(5),
            Lt,
            Int(10),
            Gt,
            Int(5),
            Semicolon,
            Int(10),
            Eq,
            Int(10),
            Semicolon,
            Int(10),
            NotEq,
            Int(9),
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
            Int(5),
            Lt,
            Int(10),
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
