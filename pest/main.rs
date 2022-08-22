#[macro_use]
extern crate pest_derive;

// use pest::iterators::{Pair, Pairs};
// use pest::prec_climber::{Assoc, Operator, PrecClimber};
use pest::Parser;

#[derive(Parser)]
#[grammar = "../pest/grammar.pest"]
pub struct GrammarParser;

fn main() {
    match GrammarParser::parse(Rule::program, include_str!("../input.txt")) {
        Ok(tree) => {
            println!("{}", tree);
        },
        Err(err) => {
            eprintln!("Parse Error {}", err);
        }
    };
}