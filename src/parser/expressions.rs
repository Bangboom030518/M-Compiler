use super::expect_single_child;
use crate::{Pair, Rule};
pub use binary::{Expression as BinaryExpression, Operator as BinaryOperator};
pub use literals::Literal;
pub use unary::{Expression as UnaryExpression, Operator as UnaryOperator};

mod binary;
mod literals;
mod unary;

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Literal(Literal),
    Binary(BinaryExpression),
    Unary(UnaryExpression),
    Identifier(String),
}

impl<'a> From<Pair<'a>> for Expression {
    fn from(pair: Pair<'a>) -> Self {
        let pair = expect_single_child(pair);
        match pair.as_rule() {
            Rule::binary_expression => Self::Binary(BinaryExpression::from(pair)),
            Rule::unary_expression => Self::Unary(UnaryExpression::from(pair)),
            Rule::literal => Self::Literal(Literal::from(pair)),
            Rule::group => Self::from(expect_single_child(pair)),
            Rule::identifier => {
                Self::Identifier(expect_single_child(pair).as_span().as_str().to_string())
            }
            rule => unreachable!("'{:?}' is not a valid expression.", rule),
        }
    }
}
