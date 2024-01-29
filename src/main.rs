use monkey::Lexer;


fn main() {
    let input = "let fi_ve = 5;";
    dbg!(input);

    let mut lexer = Lexer::new(input);

    for _ in 0..8 {
        // dbg!(&lexer);
        dbg!(lexer.next());
    }
}
