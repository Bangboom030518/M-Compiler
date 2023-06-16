use crate::prelude::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Rand)]
pub enum Operator {
    Negate,
    Bang,
}

impl std::fmt::Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let string = match self {
            Self::Negate => "-",
            Self::Bang => "!",
        };
        write!(f, "{string}")
    }
}

impl NomParse for Operator {
    fn parse(input: &str) -> IResult<Self> {
        alt((value(Self::Negate, tag("-")), value(Self::Bang, tag("!"))))(input)
    }
}
