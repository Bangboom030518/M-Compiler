use crate::{digits, prelude::*, Base};

#[derive(Debug, Clone, PartialEq, Eq, Rand)]
pub struct Hexadecimal(char);

impl Parse for Hexadecimal {
    fn parse(input: &str) -> IResult<Self> {
        delimited(
            tag(r"\u{"),
            map(digits(Base::Hexadecimal), |digits| {
                const DEFAULT: char = '�';
                let ch = Base::Hexadecimal
                    .parse_digits(digits)
                    .and_then(|number| number.try_into().ok())
                    .and_then(char::from_u32)
                    .unwrap_or(DEFAULT);
                Self(ch)
            }),
            tag("}"),
        )(input)
    }
}

impl std::fmt::Display for Hexadecimal {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "\\u{{{:x}}}", self.0 as u32)
    }
}
