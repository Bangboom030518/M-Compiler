#![feature(iter_intersperse)]
use bnf::Grammar;
use lazy_static::lazy_static;
use regex::Regex;
use std::process;

lazy_static! {
    static ref COMMENTS_PATTERN: Regex = Regex::new(r"\(\*[\s\S]*?\*\)").unwrap();
    static ref RANGE_PATTERN: Regex = Regex::new(r"\[(?P<Range>[\s\S]*?)\]").unwrap();
}

fn error(message: &str) {
    println!("{}", message);
    process::exit(1);
}

fn parse_bnf(input: &str) -> String {
    let mut input = COMMENTS_PATTERN.replace_all(input, "").to_string();
    for captures in RANGE_PATTERN.captures_iter(&input.clone()) {
        let range: &str = &captures["Range"];
        let expanded = range
            .chars()
            .collect::<Vec<char>>()
            .chunks(3)
            .map(|chunk| {
                if chunk.len() != 3 || chunk[1] != '-' {
                    error(&format!("Couldn't parse range {}", range))
                };
                (chunk[0], chunk[2])
            })
            .map(|pair| {
                (pair.0..=pair.1)
                    .map(|ch| format!("'{}'", ch))
                    .intersperse("|".to_string())
                    .collect::<String>()
            })
            .collect::<String>();
        input = input.replace(&format!("[{}]", range), &expanded);
    }
    input
}

fn main() {
    let input = parse_bnf(include_str!("../bnf/input.bnf"));
    print!("{}", input);

    let grammar: Grammar = input.parse().unwrap();

    let sentence = "0";

    let mut parse_trees = grammar.parse_input(sentence);
    match parse_trees.next() {
        Some(parse_tree) => println!("{}", parse_tree),
        None => println!("Grammar could not parse sentence"),
    }
}
