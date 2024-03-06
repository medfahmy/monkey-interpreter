use monkey::{Eval, Parser};
use std::io::{stdin, stdout, Write};

fn main() {
    let stdin = stdin();
    let mut stdout = stdout();

    let mut eval = Eval::new();

    let prompt = ">> ";

    loop {
        print!("{}", prompt);
        stdout.flush().unwrap();

        let mut buf = String::new();
        stdin.read_line(&mut buf).unwrap();

        println!();

        let program = Parser::parse(&buf);

        // println!("{:?}", program);

        let errors = program.errors();

        if !errors.is_empty() {
            println!("parser errors: ");

            for error in errors {
                println!("- {}", error);
            }

            println!();
        } else {
            println!("{}", eval.eval(program));

            // println!("{:?}", program);
            // println!();
            // println!("{}", program.to_string());
        }
    }
}
