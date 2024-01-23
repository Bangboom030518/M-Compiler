#![warn(clippy::pedantic, clippy::nursery)]
#![feature(assert_matches, iter_collect_into, if_let_guard)]

use std::fmt::Display;
use std::iter;

pub use expression::Expression;
use internal::prelude::*;
use prelude::top_level::Declaration;

pub mod expression;
pub mod parser;
pub mod scope;
pub mod top_level;

pub trait Parse {
    // TODO: errors!
    fn parse(parser: &mut Parser) -> Option<Self>
    where
        Self: Sized;
}

#[must_use]
pub fn parse_file(input: &str) -> Option<scope::File> {
    let mut parser = Parser::from(Tokenizer::from(input));
    parser.get_scope(parser.scope).declarations =
        iter::from_fn(|| parser.parse_line::<top_level::Declaration>())
            .map(|Declaration { name, kind }| (name, kind))
            .collect();
    parser.peek_eof()?;
    Some(parser.into())
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub struct Ident(pub String);

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl AsRef<str> for Ident {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

impl Parse for Ident {
    fn parse(parser: &mut Parser) -> Option<Self> {
        let Token::Ident(ident) = parser.take_token()? else {
            return None;
        };
        Some(Self(ident))
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub enum Type {
    Ident(Ident),
}

impl Type {
    pub fn ident(self) -> Ident {
        match self {
            Self::Ident(ident) => ident
        }
    }
}

impl Parse for Type {
    fn parse(parser: &mut Parser) -> Option<Self> {
        Some(Self::Ident(parser.parse()?))
    }
}

impl From<Type> for Ident {
    fn from(value: Type) -> Self {
        match value {
            Type::Ident(ident) => ident,
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Statement {
    Expression(Expression),
    Let(Ident, Expression),
    // TODO: accessors!
    Assignment(Assignment),
}

#[derive(PartialEq, Debug, Clone)]
pub struct Assignment(pub Ident, pub Expression);

impl Parse for Assignment {
    fn parse(parser: &mut Parser) -> Option<Self> {
        let left = parser.parse()?;
        parser.take_token_if(&Token::Assignment)?;
        Some(Self(left, parser.parse()?))
    }
}

impl Parse for Statement {
    fn parse(parser: &mut Parser) -> Option<Self> {
        if parser.take_token_if(&Token::Let).is_some() {
            let name = parser.parse()?;
            parser.take_token_if(&Token::Assignment)?;
            Some(Self::Let(name, parser.parse()?))
        } else {
            parser
                .parse()
                .map(Self::Assignment)
                .or_else(|| parser.parse().map(Self::Expression))
        }
    }
}

#[test]
fn test_let() {
    let source = r"let a = 1";
    let mut parser = Parser::from(Tokenizer::from(source));
    let r#let = parser.parse::<Statement>().unwrap();
    assert_eq!(
        r#let,
        Statement::Let(
            Ident(String::from("a")),
            Expression::Literal(Literal::Integer(1))
        )
    );
}

mod internal {
    pub mod prelude {
        pub use crate::prelude::*;
        pub use itertools::{Itertools, PeekingNext};
        #[cfg(test)]
        pub use std::assert_matches::assert_matches;
        pub use std::collections::HashMap;
        pub use std::iter::Peekable;
        pub use tokenizer::{Token, Tokenizer};
    }
}

pub mod prelude {
    pub use crate::expression::prelude::*;
    pub use crate::expression::{self};
    pub use crate::parser::{self, Parser};
    pub use crate::scope::{self, Scope};
    pub use crate::top_level::prelude::*;
    pub use crate::top_level::{self};
    pub use crate::{parse_file, Expression, Ident, Parse, Statement, Type};
}
