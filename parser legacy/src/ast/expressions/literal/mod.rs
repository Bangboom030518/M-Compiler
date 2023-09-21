use crate::prelude::*;
// use rand::{prelude::*, distributions::Standard};
pub use boolean::Boolean;
pub use characters::*;
pub use list::List;
pub use number::*;

mod boolean;
mod characters;
mod list;
mod number;

#[derive(Debug, Clone, PartialEq, Eq, Rand)]
pub enum Literal {
    // #[skip_variant]
    Number(Number),
    // #[skip_variant]
    List(List),
    // #[skip_variant]
    Character(Character),
    // #[skip_variant]
    String(MString),
    #[default_variant]
    Boolean(Boolean),
}

impl Parse for Literal {
    fn parse(input: &str) -> IResult<Self> {
        alt((
            map(Number::parse, Literal::Number),
            map(List::parse, Literal::List),
            map(Character::parse, Literal::Character),
            map(MString::parse, Literal::String),
            map(Boolean::parse, Literal::Boolean),
        ))(input)
    }
}

impl std::fmt::Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Number(number) => write!(f, "{number}"),
            Self::List(list) => write!(f, "{list}"),
            Self::Character(character) => write!(f, "{character}"),
            Self::String(string) => write!(f, "{string}"),
            Self::Boolean(boolean) => write!(f, "{boolean}"),
        }
    }
}
