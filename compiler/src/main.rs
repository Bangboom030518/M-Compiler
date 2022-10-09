#![warn(clippy::pedantic, clippy::nursery)]

use resolution::build_file;

fn main() {
    let tree = build_file("../../input.txt").unwrap_or_else(|error| {
        eprintln!("{}", error);
        std::process::exit(1)
    });

    dbg!(tree);
    println!("Finished parsing :)");
}
