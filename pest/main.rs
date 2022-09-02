mod parser;
mod validator;

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
    match validator::validate_pest(include_str!("../pest/grammar.pest")) {
        Ok(_) => {}
    };
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
        match node.as_rule() {
            Rule::expression => "EXPRESSION!!1!1!!",
            _ => ""
        };
        println!("{}{:?} ({})", "  ".repeat(indent), node.as_rule(), node.as_str());
        print_tree(node.into_inner(), indent + 1);
    }
}
