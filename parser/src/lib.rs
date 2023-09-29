use internal::prelude::*;

pub mod parser;
mod top_level;

pub trait Parse {
    // TODO: errors!
    fn parse<'a>(parser: &mut Parser<'a>) -> Option<Self>
    where
        Self: Sized;
}

#[derive(PartialEq, Debug, Clone)]
pub struct Identifier(String);

impl Parse for Identifier {
    fn parse<'a>(parser: &mut Parser<'a>) -> Option<Self> {
        let Token::Ident(ident) = parser.take_token()? else {
            return None;
        };
        Some(Self(ident))
    }
}

#[derive(PartialEq, Debug)]
pub enum Expression {
    Identifier(Identifier),
}

impl Parse for Expression {
    fn parse<'a>(parser: &mut Parser<'a>) -> Option<Self> {
        Some(Self::Identifier(parser.parse()?))
    }
}

mod internal {
    pub mod prelude {
        pub use crate::{
            parser::{self, Parser},
            Expression, Identifier, Parse,
        };
        pub use itertools::{Itertools, PeekingNext};
        pub use std::iter::Peekable;
        pub use tokenizer::{Token, Tokenizer};
    }
}
