#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Let,
    Fn,
    If,
    Else,
    Return,
    True,
    False,
    Ident(String),
    Int(String),
    Assign(char),
    Add,
    Sub,
    Mul,
    Div,
    Bang,
    Eq,
    NotEq,
    Lt,
    Gt,
    Semicolon,
    Comma,
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Illegal,
    Eof,
}

impl ToString for Token {
    fn to_string(&self) -> String {
        use Token::*;

        let s = match self {
            Let => "let",
            Fn => "fn",
            If => "if",
            Else => "else",
            Return => "return",
            True => "true",
            False => "false",
            Ident(s) => s,
            Int(s) => s,
            Assign(op) => match op {
                '\0' => "=",
                '+' => "+=",
                '-' => "-=",
                '*' => "*=",
                '/' => "/=",
                _ => unreachable!(),
            },
            Add => "+",
            Sub => "-",
            Mul => "*",
            Div => "/",
            Bang => "!",
            Eq => "==",
            NotEq => "!=",
            Lt => "<",
            Gt => ">",
            Semicolon => ";",
            Comma => ",",
            Lparen => "(",
            Rparen => ")",
            Lbrace => "{",
            Rbrace => "}",
            Eof => "",
            Illegal => "",
        };

        s.to_string()
    }
}
