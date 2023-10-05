#![warn(clippy::pedantic, clippy::nursery)]

pub use expression::Expression;
use internal::prelude::*;

pub mod expression;
pub mod parser;
pub mod top_level;

pub trait Parse {
    // TODO: errors!
    fn parse<'a>(parser: &mut Parser<'a>) -> Option<Self>
    where
        Self: Sized;
}

pub fn parse_file(input: &str) -> Option<Vec<top_level::Declaration>> {
    let mut parser = Parser::from(Tokenizer::from(input));
    let mut declarations = Vec::new();
    while let Some(declaration) = parser.parse_line() {
        declarations.push(declaration)
    }
    Some(declarations)
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub struct Identifier(String);

impl Parse for Identifier {
    fn parse<'a>(parser: &mut Parser<'a>) -> Option<Self> {
        let Token::Ident(ident) = parser.take_token()? else {
            return None;
        };
        Some(Self(ident))
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Type {
    Identifier(Identifier),
}

impl Parse for Type {
    fn parse<'a>(parser: &mut Parser<'a>) -> Option<Self> {
        Some(Self::Identifier(parser.parse()?))
    }
}

mod internal {
    pub mod prelude {
        pub use crate::prelude::*;
        pub use itertools::{Itertools, PeekingNext};
        pub use std::iter::Peekable;
        pub use tokenizer::{Token, Tokenizer};
    }
}

pub mod prelude {
    pub use crate::{
        parse_file,
        parser::{self, Parser},
        top_level::{self, prelude::*},
        Expression, Identifier, Parse, Type,
    };
}
