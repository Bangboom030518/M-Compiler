pub use literal::{Base, Integer, Fractional, Literal, Sign, Number};
pub use binary::{Expression as Binary, Operator as BinaryOperator};
pub use unary::{Expression as Unary, Operator as UnaryOperator};
pub use accessor::Call;

mod literal;
mod binary;
mod unary;
mod accessor;

#[derive(Debug)]
pub enum Expression {
    Literal(Literal),
    Binary(Binary),
    Unary(Unary),
    Call(Call)
}
