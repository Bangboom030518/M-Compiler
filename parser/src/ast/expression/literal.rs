use super::Expression;
pub use number::{Base, Fractional, Integer, Number, Sign};

mod number;

#[derive(Debug, Clone)]
pub enum Literal {
    Number(Number),
    List(Vec<Expression>),
    Char(char),
    String(String),
}
