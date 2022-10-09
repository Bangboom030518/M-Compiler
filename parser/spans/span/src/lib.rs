pub trait Spanned {
    fn as_span(&self) -> Span;
}

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}
