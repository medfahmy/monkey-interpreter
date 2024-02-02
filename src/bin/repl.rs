use std::io::{stdin, stdout, Write};
use monkey::Parser;


fn main() {
    let stdin = stdin();
    let mut stdout = stdout();

    let prompt = ">> ";

    loop {
        print!("{}", prompt);
        stdout.flush().unwrap();

        let mut buf = String::new();
        stdin.read_line(&mut buf).unwrap();

        println!();

        let program = Parser::parse(&buf);

        let errors = program.errors();

        if !errors.is_empty() {
            println!("errors: {:?}", errors);
        } else {
            println!("{:?}", program);
        }

        println!();
    }
}
