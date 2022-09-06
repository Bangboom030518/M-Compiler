#![allow(unused)]
#![warn(clippy::pedantic, clippy::nursery)]

mod parser;
// mod validator;

#[macro_use]
extern crate pest_derive;

use pest::error::Error;
use pest::iterators::{Pairs as PestPairs, Pair as PestPair};
use pest::Parser;
use std::process;
use parser::parse;

pub type Pairs<'a> = PestPairs<'a, Rule>;
pub type Pair<'a> = PestPair<'a, Rule>;

#[derive(Parser)]
#[grammar = "../pest/grammar.pest"]
pub struct GrammarParser;

fn tokenize(input: &str) -> Result<Pairs, Error<Rule>> {
    GrammarParser::parse(Rule::program, input)
}

fn error(message: &str) -> ! {
    eprintln!("{}", message);
    process::exit(1);
}

fn main() {
    // if let Err(message) = validator::validate_pest(include_str!("../pest/grammar.pest")) {
    //     error(&format!("Error in pest: {}", message));
    // }

    let tokens = match tokenize(include_str!("../input.txt")) {
        Ok(tokens) => tokens,
        Err(message) => error(&format!("Parse Error {}", message)),
    };

    print_tree(tokens.clone(), 0);

    let tree = parse(tokens);

    dbg!(tree);
}

fn print_tree(tree: Pairs, indent: usize) {
    for node in tree {
        match node.as_rule() {
            Rule::expression => "EXPRESSION!!1!1!!",
            _ => "",
        };
        println!(
            "{}{:?} ({})",
            "  ".repeat(indent),
            node.as_rule(),
            node.as_str()
        );
        print_tree(node.into_inner(), indent + 1);
    }
}
