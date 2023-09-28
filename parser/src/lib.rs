use internal::prelude::*;

pub mod parser;
mod top_level;

pub trait Parse {
    // TODO: errors!
    fn parse<'a>(parser: &mut Parser<'a>, _: parser::Marker) -> Option<Self>
    where
        Self: Sized;
}

#[derive(PartialEq, Debug)]
pub struct Identifier(String);

impl Parse for Identifier {
    fn parse<'a>(parser: &mut Parser<'a>, _: parser::Marker) -> Option<Self> {
        let Token::Ident(ident) = parser.peek_token()? else {
            return None;
        };
        Some(Self(ident))
    }
}

#[derive(PartialEq, Debug)]
pub enum Expression {
    Identifier(Identifier),
}

mod internal {
    pub mod prelude {
        pub use crate::{parser::{Parser, self}, Expression, Identifier, Parse};
        pub use itertools::{Itertools, PeekingNext};
        pub use std::iter::Peekable;
        pub use tokenizer::{Token, Tokenizer};
    }
}
