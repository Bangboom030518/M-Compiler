#![warn(clippy::pedantic, clippy::nursery, clippy::cargo)]

use pest::iterators::{Pair as PestPair, Pairs as PestPairs};
use pest::Parser;
use pest::error::Error;

#[macro_use]
extern crate pest_derive;

pub type Pairs<'a> = PestPairs<'a, Rule>;
pub type Pair<'a> = PestPair<'a, Rule>;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct GrammarParser;

/// Converts a string to recursive list of tokens, according to the grammar defined in grammar.pest.
/// 
/// # Errors
/// Returns an error pointing to a specific token, if that token is unable to be categorised using the grammar.
pub fn tokenize(input: &str) -> Result<Pairs, Error<Rule>> {
    GrammarParser::parse(Rule::program, input)
}

pub fn print_tree(tree: &Pairs, indent: usize) {
    for node in tree.clone() {
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
        print_tree(&node.into_inner(), indent + 1);
    }
}
