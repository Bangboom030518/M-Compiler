// TODO: implement spanned on everything

pub mod declaration;
mod expression;
mod types;

pub use declaration::Declaration;
pub use expression::{
    Base, Binary as BinaryExpression, BinaryOperator, Call as CallExpression, Expression,
    Fractional, Integer, Literal, Namespace, Number, Sign, Unary as UnaryExpression, UnaryOperator,
};
pub use types::{GenericParams, Type};

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(Expression),
    Declaration(Declaration),
}

pub trait Spanned {
    fn as_span(&self) -> Span;
}

#[derive(Debug, Clone)]
pub struct Span {
    start: usize,
    end: usize,
}
