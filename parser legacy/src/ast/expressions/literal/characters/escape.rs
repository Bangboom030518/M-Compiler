use crate::prelude::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Rand)]
pub enum Escape {
    Newline,
    LineFeed,
    Tab,
    Backslash,
    Null,
    DoubleQuote,
    SingleQuote,
}

impl Parse for Escape {
    fn parse(input: &str) -> IResult<Self> {
        alt((
            value(Self::Newline, tag(r"\n")),
            value(Self::LineFeed, tag(r"\r")),
            value(Self::Tab, tag(r"\t")),
            value(Self::Backslash, tag(r"\\")),
            value(Self::Null, tag(r"\0")),
            value(Self::DoubleQuote, tag(r#"\""#)),
            value(Self::SingleQuote, tag(r"\'")),
        ))(input)
    }
}

impl std::fmt::Display for Escape {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let string = match self {
            Self::Newline => r"\n",
            Self::LineFeed => r"\r",
            Self::Tab => r"\t",
            Self::Backslash => r"\\",
            Self::Null => r"\0",
            Self::DoubleQuote => r#"\""#,
            Self::SingleQuote => r"\'",
        };
        write!(f, "{string}")
    }
}
