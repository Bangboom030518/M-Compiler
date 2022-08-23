#[macro_use]
extern crate pest_derive;

use pest::iterators::Pairs;
use pest::error::Error;
use pest::Parser;

#[derive(Parser)]
#[grammar = "../pest/grammar.pest"]
pub struct GrammarParser;

fn parse(input: &str) -> Result<Pairs<Rule>, Error<Rule>> {
    GrammarParser::parse(Rule::program, input)
}

fn main() {
    match parse(include_str!("../input.txt")) {
        Ok(tree) => {
            print_tree(tree, 0);
        },
        Err(err) => {
            eprintln!("Parse Error {}", err);
        }
    };
}

fn print_tree(tree: Pairs<Rule>, indent: usize) {
    for node in tree {
        println!("{}{:?} ({})", "  ".repeat(indent), node.as_rule(), node.as_str());
        print_tree(node.into_inner(), indent + 1);
    }
}