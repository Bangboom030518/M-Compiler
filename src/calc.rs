#[macro_use]
extern crate pest_derive;

use lazy_static::lazy_static;
use pest::iterators::{Pair, Pairs};
use pest::prec_climber::{Assoc, Operator, PrecClimber};
use pest::Parser;
use std::u64;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct CalculatorParser;

lazy_static! {
    static ref PREC_CLIMBER: PrecClimber<Rule> = {
        use Assoc::*;
        use Rule::*;

        PrecClimber::new(vec![
            Operator::new(add, Left) | Operator::new(subtract, Left),
            Operator::new(multiply, Left) | Operator::new(divide, Left),
            Operator::new(power, Right),
        ])
    };
}

fn eval(expression: Pairs<Rule>) -> f64 {
    PREC_CLIMBER.climb(
        expression,
        |pair: Pair<Rule>| match pair.as_rule() {
            Rule::number => {
                let number = pair.into_inner().next().unwrap();
                match number.as_rule() {
                    Rule::denary => number.as_str().parse::<f64>().unwrap(),
                    _ => unreachable!()
                }
            },
            Rule::expr => eval(pair.into_inner()),
            _ => unreachable!(),
        },
        |lhs: f64, op: Pair<Rule>, rhs: f64| match op.as_rule() {
            Rule::add => lhs + rhs,
            Rule::subtract => lhs - rhs,
            Rule::multiply => lhs * rhs,
            Rule::divide => lhs / rhs,
            Rule::power => lhs.powf(rhs),
            _ => unreachable!(),
        },
    )
}

fn main() {
    match CalculatorParser::parse(Rule::calculation, include_str!("main.test")) {
        Ok(tree) => {
            println!("{}", eval(tree));
        }
        Err(err) => {
            eprintln!("Parse Error {}", err);
        }
    };
}
