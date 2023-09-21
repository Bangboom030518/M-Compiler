use super::Identifier;
use crate::prelude::*;

#[derive(Clone, Debug, PartialEq, Eq, Rand)]
pub enum Pattern {
    Identifier(Identifier),
}

impl Parse for Pattern {
    fn parse(input: &str) -> IResult<Self> {
        map(Identifier::parse, Self::Identifier)(input)
    }
}

impl std::fmt::Display for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Identifier(identifier) => write!(f, "{identifier}"),
        }
    }
}
