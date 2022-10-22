use super::{Sign, Base};
use span::Span;
use span_derive::Span;

#[derive(Debug, Clone, Span)]
pub struct Float {
    pub base: Base,
    pub whole_digits: Vec<usize>,
    pub fractional_digits: Vec<usize>,
    pub sign: Sign,
    pub data_type: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Type {
    Float32Bit,
    Float64Bit,
}
