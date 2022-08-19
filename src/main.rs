use std::fs;

mod tokenizer;
mod parser;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Failed to read file 'input.txt'");
    let ref tokens = tokenizer::tokenize(&input);
    for token in tokens {
        println!("{:?}", token);
    }
    let ast = parser::parse(&tokens);
    dbg!(ast);
}
