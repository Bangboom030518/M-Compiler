#![warn(clippy::pedantic, clippy::nursery)]

use parser::{tokenize, parse};

fn main() {
    let tokens = match tokenize(include_str!("../../input.txt")) {
        Ok(tokens) => tokens,
        Err(message) => panic!("Parse Error {}", message),
    };

    dbg!(&tokens);

    let tree = parse(tokens);

    dbg!(tree);
    println!("Finished parsing :)");
}
