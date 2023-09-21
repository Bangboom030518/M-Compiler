use crate::prelude::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Rand)]
pub enum Operator {
    Multiply,
    Add,
    Subtract,
    Divide,
    Remainder,
    Exponent,
    Equal,
    NotEqual,
    GreaterThan,
    LessThan,
    GreaterThanOrEqual,
    LessThanOrEqual,
}

impl Operator {
    #[must_use]
    pub const fn binding_powers(self) -> (u8, u8) {
        match self {
            Self::Equal
            | Self::NotEqual
            | Self::GreaterThan
            | Self::LessThan
            | Self::GreaterThanOrEqual
            | Self::LessThanOrEqual => (0, 1),

            Self::Add | Self::Subtract => (2, 3),
            Self::Multiply | Self::Divide | Self::Remainder => (4, 5),
            Self::Exponent => (6, 7),
        }
    }
}

impl Parse for Operator {
    fn parse(input: &str) -> IResult<Self> {
        alt((
            value(Self::Exponent, tag("**")),
            value(Self::Add, tag("+")),
            value(Self::Subtract, tag("-")),
            value(Self::Multiply, tag("*")),
            value(Self::Divide, tag("/")),
            value(Self::Remainder, tag("%")),
            value(Self::Equal, tag("==")),
            value(Self::NotEqual, tag("!=")),
            value(Self::GreaterThan, tag(">")),
            value(Self::LessThan, tag("<")),
            value(Self::GreaterThanOrEqual, tag(">=")),
            value(Self::LessThanOrEqual, tag("<=")),
        ))(input)
    }
}

impl std::fmt::Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let string = match self {
            Self::Add => "+",
            Self::Subtract => "-",
            Self::Multiply => "*",
            Self::Divide => "/",
            Self::Remainder => "%",
            Self::Exponent => "**",
            Self::Equal => "==",
            Self::NotEqual => "!=",
            Self::GreaterThan => ">",
            Self::LessThan => "<",
            Self::GreaterThanOrEqual => ">=",
            Self::LessThanOrEqual => "<=",
        };
        write!(f, "{string}")
    }
}
