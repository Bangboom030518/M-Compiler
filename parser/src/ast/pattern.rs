use super::Identifier;
use span_derive::Span;

#[derive(Clone, Debug, Span)]
pub enum Pattern {
    Identifier(Identifier),
}
