mod base;
mod float;
mod integer;
mod prefix;
mod sign;

use crate::prelude::*;
pub use base::Base;
pub use float::{Float, Type as FloatType};
pub use integer::{Integer, Type as IntegerType};
use prefix::Prefix;
pub use sign::Sign;

fn digit<'a>(base: Base) -> impl FnMut(&'a str) -> IResult<usize> {
    map(one_of(base.get_accepted_digits()), move |digit| {
        base.get_digits()
            .iter()
            .position(|&character| character == digit.to_ascii_lowercase())
            .unwrap()
    })
}

pub fn digits<'a>(base: Base) -> impl FnMut(&'a str) -> IResult<Vec<usize>> {
    many0(terminated(digit(base), many0(char('_'))))
}

#[derive(Debug, Clone, PartialEq, Eq, Rand)]
pub enum Number {
    Integer(Integer),
    Float(Float),
}

impl NomParse for Number {
    fn parse(input: &str) -> IResult<Self> {
        alt((
            map(Integer::parse, Self::Integer),
            map(Float::parse, Self::Float),
        ))(input)
    }
}

impl std::fmt::Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Integer(integer) => write!(f, "{integer}"),
            Self::Float(float) => write!(f, "{float}"),
        }
    }
}
