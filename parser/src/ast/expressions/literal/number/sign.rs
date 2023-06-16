use crate::prelude::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Rand)]
pub enum Sign {
    Negative,
    Positive,
}

impl NomParse for Sign {
    fn parse(input: &str) -> IResult<Self> {
        alt((value(Self::Negative, char('-')), success(Self::Positive)))(input)
    }
}

impl Default for Sign {
    fn default() -> Self {
        Self::Positive
    }
}

impl std::fmt::Display for Sign {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Positive => write!(f, ""),
            Self::Negative => write!(f, "-"),
        }
    }
}
