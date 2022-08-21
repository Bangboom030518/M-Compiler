extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::Parser;
use std::fs;

#[derive(Parser)]
#[grammar = "../pest/csv.pest"]
pub struct CSVParser;

fn main() {
    let unparsed_file = include_str!("../pest/test.csv");

    // let file = CSVParser::parse(Rule::file, &unparsed_file)
    //     .expect("unsuccessful parse") // unwrap the parse result
    //     .next().unwrap(); // get and unwrap the `file` rule; never fails

    // dbg!(file);
    match CSVParser::parse(Rule::file, &unparsed_file) {
        Ok(result) => {
            dbg!(result);
        }
        Err(err) => {
            eprintln!("Parse Error {}", err)
        }
    }
}
