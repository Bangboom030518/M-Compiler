#![warn(clippy::pedantic, clippy::nursery)]

// TODO: refactor into seperate modules
pub mod expressions;
pub mod declarations;

pub use tokenizer::{Pair, Pairs, Rule, tokenize};
use expressions::Expression;
use declarations::Declaration;

#[derive(Debug, PartialEq)]
pub struct Program {
    pub body: Vec<Statement>,
}

impl<'a> From<Pairs<'a>> for Program {
    fn from(pairs: Pairs<'a>) -> Self {
        let body = pairs
            .filter_map(|pair| match pair.as_rule() {
                Rule::statement => Some(Statement::from(pair)),
                Rule::EOI => None,
                _ => unreachable!(
                    "Expected only statements at top level. Found '{:?}'",
                    pair.as_rule()
                ),
            })
            .collect::<Vec<Statement>>();

        Self { body }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Expression(Expression),
    Declaration(Declaration),
}

impl<'a> From<Pair<'a>> for Statement {
    fn from(pair: Pair<'a>) -> Self {
        let pair = expect_single_child(pair);
        match pair.as_rule() {
            Rule::expression => Self::Expression(Expression::from(pair)),
            Rule::declaration => Self::Declaration(Declaration::from(pair)),
            rule => unreachable!(
                "Statement can only be an expression or declaration. Found '{:?}'",
                rule
            ),
        }
    }
}

pub fn parse(pairs: Pairs) -> Program {
    Program::from(pairs)
}

fn expect_single_child(pair: Pair) -> Pair {
    let rule = pair.as_rule();
    let mut pairs = pair.into_inner();
    pairs
        .next()
        .and_then(|pair| {
            if pairs.next().is_none() {
                Some(pair)
            } else {
                None
            }
        })
        .unwrap_or_else(|| panic!("Rule '{:?}' should have exactly 1 child", rule))
}
