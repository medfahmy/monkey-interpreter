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
