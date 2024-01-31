use std::io::{stdin, stdout, Write};
use monkey::{Lexer, Parser};


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

        let lexer = Lexer::new(buf);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();

        let errors = parser.errors();

        // todo: impl Display for Parser and Program
        
        if errors.len() > 0 {
            println!("errors: {:?}", errors);
        } else {
            println!("{:?}", program);
        }

        println!("");
    }
}
