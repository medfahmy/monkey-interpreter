use monkey::{Env, Eval, Parser};
use std::io::{stdin, stdout, Write};

fn main() {
    repl();
}

fn repl() {
    let stdin = stdin();
    let mut stdout = stdout();
    let mut env = Env::new();

    loop {
        print!(">> ");
        stdout.flush().unwrap();
        let mut buf = String::new();
        stdin.read_line(&mut buf).unwrap();
        let program = Parser::parse(&buf);
        let errors = program.errors();

        if errors.is_empty() {
            println!("{}", program.eval(&mut env));
        } else {
            println!("parser errors: ");

            for error in errors {
                println!("- {}", error);
            }
        }
    }
}
