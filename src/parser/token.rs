#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Ident(String),
    Int(String),
    Str(String),
    Assign(char),
    Let,
    Fn,
    If,
    Else,
    Return,
    True,
    False,
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
    Lbracket,
    Rbracket,
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
            Str(s) => s,
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
            Lbracket => "[",
            Rbracket => "]",
            Eof => "",
            Illegal => "",
        };

        s.to_string()
    }
}
