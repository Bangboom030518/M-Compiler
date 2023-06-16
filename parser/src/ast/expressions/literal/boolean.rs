use crate::prelude::*;
use rand_derive::Rand;

#[derive(Debug, Clone, PartialEq, Eq, Rand)]
pub struct Boolean(bool);

impl NomParse for Boolean {
    fn parse(input: &str) -> IResult<Self> {
        map(
            alt((value(true, tag("true")), value(false, tag("false")))),
            Self,
        )(input)
    }
}

impl std::fmt::Display for Boolean {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
