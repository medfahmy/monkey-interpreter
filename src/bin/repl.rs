use monkey::{Env, Eval, Parser};
use std::io::{stdin, stdout, Write};

fn main() {
    let stdin = stdin();
    let mut stdout = stdout();
    let mut env = Env::new();
    let prompt = ">> ";

    loop {
        print!("{}", prompt);
        stdout.flush().unwrap();

        let mut buf = String::new();
        stdin.read_line(&mut buf).unwrap();

        println!();

        let program = Parser::parse(&buf);
        let errors = program.errors();

        if errors.is_empty() {
            // println!("{:?}", program.stmts());
            println!("{}", program.eval(&mut env));
        } else {
            println!("parser errors: ");

            for error in errors {
                println!("- {}", error);
            }
        }

        println!();
    }
}
