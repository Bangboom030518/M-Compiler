use super::CodePoint;
use crate::prelude::*;

#[derive(Debug, Clone, PartialEq, Eq, Rand)]
pub struct Character(pub CodePoint);

impl Parse for Character {
    fn parse(input: &str) -> IResult<Self> {
        map(
            delimited(
                char('\''),
                preceded(peek(none_of("'")), CodePoint::parse),
                char('\''),
            ),
            Self,
        )(input)
    }
}

impl std::fmt::Display for Character {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "'{}'", self.0)
    }
}
