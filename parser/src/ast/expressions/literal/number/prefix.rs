use super::{Base, Sign};
use crate::prelude::*;

#[derive(Debug, Clone, PartialEq, Eq, Rand)]
pub struct Prefix {
    pub sign: Sign,
    pub base: Base,
}

impl Prefix {
    pub const fn new(sign: Sign, base: Base) -> Self {
        Self { sign, base }
    }
}

impl std::fmt::Display for Prefix {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}{}", self.sign, self.base)
    }
}

impl NomParse for Prefix {
    fn parse(input: &str) -> IResult<Self> {
        map(tuple((Sign::parse, Base::parse)), |(sign, base)| {
            Self::new(sign, base)
        })(input)
    }
}
