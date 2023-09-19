use super::{digits, Base, Prefix, Sign};
use crate::prelude::*;

#[derive(Default, Debug, Clone, PartialEq, Eq, Rand)]
pub struct Float {
    pub sign: Sign,
    pub base: Base,
    pub whole_digits: Vec<usize>,
    pub fractional_digits: Vec<usize>,
    pub data_type: Type,
}

impl Parse for Float {
    fn parse(input: &str) -> IResult<Self> {
        let (input, Prefix { sign, base }) = Prefix::parse(input)?;
        map(
            tuple((opt(digits(base)), char('.'), digits(base), Type::parse)),
            move |(whole_digits, _, fractional_digits, data_type)| Self {
                sign,
                base,
                whole_digits: whole_digits.unwrap_or_default(),
                fractional_digits,
                data_type,
            },
        )(input)
    }
}

impl std::fmt::Display for Float {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let Self {
            sign,
            base,
            whole_digits,
            fractional_digits,
            data_type,
        } = self;

        write!(
            f,
            "{sign}{base}{}.{}{data_type}",
            base.fmt_digits(whole_digits),
            base.fmt_digits(fractional_digits)
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Rand)]
pub enum Type {
    Float32,
    Float64,
}

impl Default for Type {
    fn default() -> Self {
        Self::Float64
    }
}

impl Parse for Type {
    fn parse(input: &str) -> IResult<Self> {
        preceded(
            char(':'),
            alt((
                value(Self::Float32, tag("f32")),
                value(Self::Float64, tag("f64")),
            )),
        )(input)
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let string = match self {
            Self::Float32 => "f32",
            Self::Float64 => "f64",
        };
        write!(f, ":{string}")
    }
}
