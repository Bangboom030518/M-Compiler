#![warn(clippy::pedantic, clippy::nursery)]

use parser::parse;
use resolution::build_file;

fn main() {
    let tree = build_file("../../input.txt");

    dbg!(tree);
    println!("Finished parsing :)");
}
