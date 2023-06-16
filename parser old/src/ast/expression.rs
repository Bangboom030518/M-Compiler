pub mod accessor;
pub mod binary;
pub mod literal;
pub mod unary;

pub use literal::Literal;

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(Literal),
    Binary(binary::Expression),
    Unary(unary::Expression),
    Call(accessor::Call),
    Namespace(accessor::Namespace),
    Identifier(super::Identifier),
}
