// TODO: implement spanned on everything

pub mod data_type;
pub mod declaration;
pub mod expression;
pub mod pattern;

pub use data_type::Type;
pub use expression::Expression;
use span::Span;
use span_derive::Span;

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(expression::Expression),
    Declaration(declaration::local::Declaration),
    Continue,
    Break,
    Return(expression::Expression),
}

#[derive(Clone, Debug, Span)]
pub struct Identifier {
    name: String,
    span: Span,
}
