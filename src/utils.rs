use std::process;

pub fn error(message: &str) {
    println!("{}", message);
    process::exit(1);
}