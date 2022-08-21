use bnf::Grammar;
use lazy_static::lazy_static;
use regex::Regex;
use std::{fs, process};

lazy_static! {
    static ref COMMENTS_PATTERN: Regex = Regex::new(r"\(\*[\s\S]*?\*\)").unwrap();
    static ref RANGE_PATTERN: Regex = Regex::new(r"\[(?P<Range>[\s\S]*?)\]").unwrap();
}

fn error(message: &str) {
    println!("{}", message);
    process::exit(1);
}

fn parse_bnf(input: &str) -> String {
    let input = COMMENTS_PATTERN.replace_all(input, "rep").to_string();

    for captures in RANGE_PATTERN.captures_iter(&input) {
        let range = &captures["Range"];
        range
            .chars()
            .collect::<Vec<char>>()
            .chunks(3)
            .map(|chunk| {
                
                (chunk[0], chunk[2])
            });
    }
    input
}

fn main() {
    let input = parse_bnf(&fs::read_to_string("input.bnf").unwrap());
    // print!("{}", input);

    let grammar: Grammar = input.parse().unwrap_or_else(|_| Grammar::new());

    let sentence = "0";

    let mut parse_trees = grammar.parse_input(sentence);
    match parse_trees.next() {
        Some(parse_tree) => println!("{}", parse_tree),
        _ => println!("Grammar could not parse sentence"),
    }
}
