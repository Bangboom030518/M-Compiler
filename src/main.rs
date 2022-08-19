use std::fs;

mod tokenizer;
mod parser;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Failed to read file 'input.txt'");
    let tokens = tokenizer::tokenize(&input);
    let ast = parser::parse(&tokens);
    
    println!("{:?}", ast)
}
