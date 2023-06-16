use super::{escape::Escape, hexadecimal::Hexadecimal};
use crate::prelude::*;

#[derive(Debug, Clone, PartialEq, Eq, Rand)]
pub enum CodePoint {
    Escape(Escape),
    Hexadecimal(Hexadecimal),
    Inline(char),
}

impl NomParse for CodePoint {
    fn parse(input: &str) -> IResult<Self> {
        alt((
            map(Escape::parse, Self::Escape),
            map(Hexadecimal::parse, Self::Hexadecimal),
            map(anychar, Self::Inline),
        ))(input)
    }
}

impl std::fmt::Display for CodePoint {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Escape(character) => write!(f, "{character}"),
            Self::Hexadecimal(character) => write!(f, "{character}"),
            Self::Inline(character) => write!(f, "{character}"),
        }
    }
}
