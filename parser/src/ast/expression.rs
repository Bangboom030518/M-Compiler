pub use accessor::{Call, Namespace};
pub use binary::{Expression as Binary, Operator as BinaryOperator};
pub use literal::{Base, Fractional, Integer, Literal, Number, Sign};
pub use unary::{Expression as Unary, Operator as UnaryOperator};

pub mod accessor;
mod binary;
mod literal;
mod unary;

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(Literal),
    Binary(Binary),
    Unary(Unary),
    Call(Call),
    Identifier(String),
    Namespace(Namespace),
}
