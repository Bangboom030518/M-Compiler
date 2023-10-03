#![warn(clippy::pedantic, clippy::nursery)]

use internal::prelude::*;
pub use expression::Expression;

pub mod expression;
pub mod parser;
mod top_level;

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
    dbg!(parser);
    Some(declarations)
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
        pub use crate::{
            parser::{self, Parser},
            Expression, Identifier, Parse, Type
        };
        pub use itertools::{Itertools, PeekingNext};
        pub use std::iter::Peekable;
        pub use tokenizer::{Token, Tokenizer};
    }
}
