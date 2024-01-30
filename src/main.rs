use std::io::{stdin, stdout, Write};
use monkey::{Lexer, Token};


fn main() {
    let stdin = stdin();
    let mut stdout = stdout();

    let prompt = ">> ";

    loop {
        print!("{}", prompt);
        stdout.flush().unwrap();

        let mut buf = String::new();
        stdin.read_line(&mut buf).unwrap();

        println!("");

        let mut lexer = Lexer::new(&buf);
        let mut token = lexer.next();

        while token != Token::Eof {
            println!("{:?}", token);
            token = lexer.next();
        }

        println!("");
    }
}
