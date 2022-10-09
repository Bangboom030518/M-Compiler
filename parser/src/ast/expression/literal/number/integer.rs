use super::{Sign, Base};
use span::Span;
use span_derive::Span;

#[derive(Debug, Clone, Span)]
pub struct Integer {
    pub base: Base,
    pub digits: Vec<usize>,
    pub sign: Sign,
    pub data_type: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Type {
    Unsigned8Bit,
    Unsigned16Bit,
    Unsigned32Bit,
    Unsigned64Bit,
    Unsigned128Bit,
    Signed8Bit,
    Signed16Bit,
    Signed32Bit,
    Signed64Bit,
    Signed128Bit,
}
