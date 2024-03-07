#![warn(
    clippy::pedantic,
    clippy::nursery,
    clippy::unwrap_used,
    clippy::unreachable
)]
#![feature(assert_matches, iter_collect_into, if_let_guard)]

pub use expression::Expression;
use internal::prelude::*;
use std::collections::HashSet;
use std::fmt::Display;
use tokenizer::{AsSpanned, Spanned, SpannedResultExt, TokenType};

pub mod expression;
pub mod parser;
pub mod scope;
pub mod top_level;

#[derive(Clone, Debug, thiserror::Error)]
#[error("Unexpected token '{found:?}'. Expected one of '{expected:?}'")]
pub struct Error<'a> {
    pub expected: &'a HashSet<TokenType>,
    pub found: Spanned<&'a Token>,
}

pub trait Parse {
    fn parse(parser: &mut Parser) -> Result<Spanned<Self>, Error>
    where
        Self: Sized;
}

#[must_use]
pub fn parse_file(input: &str) -> Result<scope::File, Error> {
    let mut parser = Parser::from(Tokenizer::from(input));
    let declarations = &mut parser.get_scope(parser.scope).declarations;
    loop {
        let declaration = parser.parse::<top_level::Declaration>()?;
        declarations.insert(declaration.value.name().to_string(), declaration.value);
        if parser.peek_token_if(TokenType::Eoi).is_ok() {
            return Ok(parser.into());
        };
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident {
    pub ident: String,
}

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.ident)
    }
}

impl AsRef<str> for Ident {
    fn as_ref(&self) -> &str {
        &self.ident
    }
}

impl Parse for Ident {
    fn parse(parser: &mut Parser) -> Result<Spanned<Self>, Error> {
        parser.take_ident().map_spanned(|ident| Self {
            ident: ident.to_string(),
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Ident(Ident),
}

impl Type {
    #[must_use]
    pub fn ident(self) -> Ident {
        match self {
            Self::Ident(ident) => ident,
        }
    }
}

impl Parse for Type {
    fn parse(parser: &mut Parser) -> Result<Spanned<Self>, Error> {
        parser.parse().map_spanned(Self::Ident)
    }
}

impl From<Type> for Ident {
    fn from(value: Type) -> Self {
        match value {
            Type::Ident(ident) => ident,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Let {
    pub ident: Spanned<Ident>,
    pub expression: Spanned<Expression>,
}

impl Parse for Let {
    fn parse(parser: &mut Parser) -> Result<Spanned<Self>, Error> {
        let start = parser.take_token_if(TokenType::Let)?.span.start;
        let ident = parser.parse()?;
        parser.take_token_if(TokenType::Assignment)?;
        let expression = parser.parse()?;
        Ok(Self { ident, expression }.spanned(start..expression.end()))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Expression(Expression),
    Let(Let),
    // TODO: accessors!
    Assignment(Assignment),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assignment {
    pub left: Spanned<Expression>,
    pub right: Spanned<Expression>,
}

impl Parse for Assignment {
    fn parse(parser: &mut Parser) -> Result<Spanned<Self>, Error> {
        let left = parser.parse()?;
        parser.take_token_if(TokenType::Assignment)?;
        let right = parser.parse()?;
        Ok(Self { left, right }.spanned(left.start()..right.end()))
    }
}

impl Parse for Statement {
    fn parse(parser: &mut Parser) -> Result<Spanned<Self>, Error> {
        parser
            .parse()
            .map_spanned(Self::Let)
            .or_else(|_| parser.parse().map_spanned(Self::Assignment))
            .or_else(|_| parser.parse().map_spanned(Self::Expression))
    }
}

#[cfg(ignore)]
#[test]
fn test_let() {
    let source = r"let a = 1";
    let mut parser = Parser::from(Tokenizer::from(source));
    let r#let = parser.parse::<Statement>().unwrap();
    assert_matches!(
        r#let,
        Statement::Let(Let {
            ident: Ident { ident, .. },
            expression: Expression::Literal(Literal::Integer(int)),
            ..
         }) if ident == String::from("a") && int == 1
    );
}

mod internal {
    pub mod prelude {
        pub use crate::prelude::*;
        pub use itertools::Itertools;
        #[cfg(test)]
        pub use std::assert_matches::assert_matches;
        pub use std::collections::HashMap;
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
