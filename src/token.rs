#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // keywords
    Let,
    Fn,
    If,
    Else,
    Return,
    True,
    False,
    Ident(String),
    Int(String),
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
            Assign => "=",
            Plus => "+",
            Minus => "-",
            Bang => "!",
            Asterisk => "*",
            Slash => "/",
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
