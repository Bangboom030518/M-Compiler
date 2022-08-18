use std::fs;

mod tokenizer;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Failed to read file 'input.txt'");
    for token in tokenizer::tokenize(&input) {
        println!("{:?}", token)
    }
}
