use super::Expression;
pub use number::{Base, Integer, Fractional, Sign, Number};

mod number;

#[derive(Debug)]
pub enum Literal {
    Number(Number),
    List(Vec<Expression>),
    Char(char),
    String(String),
}

