use super::{digits, Base, Prefix, Sign};
use crate::prelude::*;

#[derive(Debug, Clone, PartialEq, Eq, Default, Rand)]
pub struct Integer {
    pub sign: Sign,
    pub base: Base,
    pub digits: Vec<usize>,
    pub data_type: Type,
}

impl NomParse for Integer {
    fn parse(input: &str) -> IResult<Self> {
        let (input, Prefix { sign, base }) = Prefix::parse(input)?;

        map(
            tuple((digits(base), Type::parse)),
            move |(digits, data_type)| Self {
                sign,
                base,
                digits,
                data_type,
            },
        )(input)
    }
}

impl std::fmt::Display for Integer {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let Self {
            sign,
            base,
            digits,
            data_type,
        } = self;
        write!(f, "{sign}{base}{}{data_type}", base.fmt_digits(digits))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Rand)]
pub enum Type {
    Unsigned8,
    Unsigned16,
    Unsigned32,
    Unsigned64,
    Unsigned128,
    Signed8,
    Signed16,
    Signed32,
    Signed64,
    Signed128,
}

impl NomParse for Type {
    fn parse(input: &str) -> IResult<Self> {
        preceded(
            char(':'),
            alt((
                value(Self::Unsigned8, tag("u8")),
                value(Self::Unsigned16, tag("u16")),
                value(Self::Unsigned32, tag("u32")),
                value(Self::Unsigned64, tag("u64")),
                value(Self::Unsigned128, tag("u128")),
                value(Self::Signed8, tag("i8")),
                value(Self::Signed16, tag("i16")),
                value(Self::Signed32, tag("i32")),
                value(Self::Signed64, tag("i64")),
                value(Self::Signed128, tag("i128")),
            )),
        )(input)
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let string = match self {
            Self::Unsigned8 => "u8",
            Self::Unsigned16 => "u16",
            Self::Unsigned32 => "u32",
            Self::Unsigned64 => "u64",
            Self::Unsigned128 => "u128",
            Self::Signed8 => "i8",
            Self::Signed16 => "i16",
            Self::Signed32 => "i32",
            Self::Signed64 => "i64",
            Self::Signed128 => "i128",
        };
        write!(f, ":{string}")
    }
}

impl Default for Type {
    fn default() -> Self {
        Self::Signed32
    }
}

impl TryFrom<Integer> for isize {
    type Error = ();

    fn try_from(value: Integer) -> Result<Self, Self::Error> {
        let unsigned = value
            .base
            .parse_digits(value.digits)
            .map_or_else(|| Err(()), Ok)?;
        match value.sign {
            Sign::Positive => Ok(unsigned as isize), 
            Sign::Negative => Ok(unsigned as isize * -1),
        }
    }
}
