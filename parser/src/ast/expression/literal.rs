use super::Expression;

pub mod number;
pub use number::Number;

#[derive(Debug, Clone)]
pub enum Literal {
    Number(Number),
    List(Vec<Expression>),
    Char(char),
    String(String),
}
