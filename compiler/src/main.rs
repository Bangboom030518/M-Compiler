#![warn(clippy::pedantic, clippy::nursery)]

use parser::parse;

fn main() {
    let tree = parse(include_str!("../../input.txt"));

    dbg!(tree);
    println!("Finished parsing :)");
}
