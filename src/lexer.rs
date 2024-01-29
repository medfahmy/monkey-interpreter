#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    Ident(&'a str),
    Int(i64),
    // Float(f64),
    Let,
    Fn,
    Assign,
    Plus,
    Comma,
    Semicolon,
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Illegal,
    Whitespace,
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
        use Token::*;

        let token = match self.curr {
            '\0' => Eof,
            '=' => Assign,
            '+' => Plus,
            '(' => Lparen,
            ')' => Rparen,
            '{' => Lbrace,
            '}' => Rbrace,
            ';' => Semicolon,
            ',' => Comma,
            ws if ws.is_whitespace() => {
                return self.skip_whitespace();
            },
            // TODO:
            // handle error
            n if n.is_digit(10) => Int(n.to_digit(10).unwrap().into()),
            _ => {
                if self.curr.is_alphabetic() || self.curr == '_' {
                    return self.read_ident();
                } else {
                    Illegal
                }
            },
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

        while is_letter(self.curr) {
            self.read_char();
        }

        let s = &self.input[position..self.position];

        use Token::*;

        match s {
            "let" => Let,
            "fn" => Fn,
            s => Ident(s),
        }
    }

    fn skip_whitespace(&mut self) -> Token {
        while self.curr.is_whitespace() {
            self.read_char();
        }

        self.next()
    }
}

fn is_letter(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}

// impl Iterator for Lexer<'_> {
//     type Item = Token>;
//
//     fn next(&mut self) -> Option<Self::Item> {
//         let token = self.next_token();
//
//         if token == Token::Eof {
//             None
//         } else {
//             Some(token)
//         }
//     }
// }

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_next_works() {
        let input = "=+(){},;";
        let mut lexer = Lexer::new(input);

        use Token::*;
        let tokens = [Assign, Plus, Lparen, Rparen, Lbrace, Rbrace, Comma, Semicolon, Eof];

        for expected_token in tokens {
            let token = lexer.next();
            assert_eq!(token, expected_token);
        }
    }

    #[test]
    fn next_works() {
        let input = r#"let five = 5;
let ten = 10;

let add = fn(x, y) {
    x + y;
};

let result = add(five, ten);"#;

        let mut lexer = Lexer::new(input);

        use Token::*;
        let tokens = [
            Let, Ident("five"), Assign, Int(5), Semicolon,
            Let, Ident("ten"), Assign, Int(10), Semicolon,
            Let, Ident("add"), Assign, Fn, Lparen, Ident("x"), Comma, Ident("y"), Rparen, Lbrace,
            Ident("x"), Plus, Ident("y"), Semicolon, Rbrace, Semicolon,
            Let, Ident("result"), Assign, Ident("add"), Rparen, Ident("five"), Ident("ten"), Rparen, Semicolon, Eof
        ];

        for expected_token in tokens {
            let token = lexer.next();
            assert_eq!(token, expected_token);
        }
    }
}
