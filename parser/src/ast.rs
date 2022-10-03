mod expression;
mod types;

pub use expression::{
    Base, Binary as BinaryExpression, BinaryOperator, Expression, Fractional, Integer, Literal,
    Number, Sign, Unary as UnaryExpression, UnaryOperator, Call as CallExpression
};

#[derive(Debug)]
pub enum Statement {
    Expression(Expression),
}
