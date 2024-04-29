use monkey::{Lexer, Token};

fn main() {
    let input = r#""hello""#;
    let mut lexer = Lexer::new(input);
    let mut curr = lexer.next();

    while curr != Token::Eof {
        println!("{:?}", curr);
        curr = lexer.next();
    }
}