use super::CodePoint;
use crate::prelude::*;

#[derive(Debug, Clone, PartialEq, Eq, Rand)]
pub struct String(pub Vec<CodePoint>);

impl NomParse for String {
    fn parse(input: &str) -> IResult<Self> {
        delimited(
            char('"'),
            map(many0(preceded(peek(none_of("\"")), CodePoint::parse)), Self),
            char('"'),
        )(input)
    }
}

impl std::fmt::Display for String {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "\"{}\"",
            self.0
                .iter()
                .map(ToString::to_string)
                .collect::<std::string::String>()
        )
    }
}
