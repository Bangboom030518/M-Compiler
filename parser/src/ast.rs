mod expression;
mod types;
pub mod declaration;

pub use expression::{
    Base, Binary as BinaryExpression, BinaryOperator, Expression, Fractional, Integer, Literal,
    Number, Sign, Unary as UnaryExpression, UnaryOperator, Call as CallExpression, Namespace
};
pub use types::{GenericParams, Type};
pub use declaration::Declaration;

#[derive(Debug)]
pub enum Statement {
    Expression(Expression),
    Declaration(Declaration)
}
