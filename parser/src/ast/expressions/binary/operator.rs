use crate::prelude::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Rand)]
pub enum Operator {
    Multiply,
    Add,
    Subtract,
    Divide,
    Modulo,
    Exponent,
}

impl Operator {
    pub const fn binding_powers(self) -> (u8, u8) {
        match self {
            Self::Add | Self::Subtract => (0, 1),
            Self::Multiply | Self::Divide | Self::Modulo => (2, 3),
            Self::Exponent => (4, 5),
        }
    }
}

impl NomParse for Operator {
    fn parse(input: &str) -> IResult<Self> {
        alt((
            value(Self::Exponent, tag("**")),
            value(Self::Add, tag("+")),
            value(Self::Subtract, tag("-")),
            value(Self::Multiply, tag("*")),
            value(Self::Divide, tag("/")),
            value(Self::Modulo, tag("%")),
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
            Self::Modulo => "%",
            Self::Exponent => "**",
        };
        write!(f, "{string}")
    }
}
