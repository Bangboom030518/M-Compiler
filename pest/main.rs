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
            println!("{}", tree);
        },
        Err(err) => {
            eprintln!("Parse Error {}", err);
        }
    };
}