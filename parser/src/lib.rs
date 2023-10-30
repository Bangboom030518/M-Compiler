#![warn(clippy::pedantic, clippy::nursery)]
#![feature(assert_matches)]

pub use expression::Expression;
use internal::prelude::*;

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
    let scope_id = parser.create_scope();
    while let Some(declaration) = parser.parse_line::<top_level::Declaration>() {
        parser
            .get_scope(scope_id)
            .declarations
            .insert(declaration.name, declaration.kind);
    }
    parser.exit_scope();

    Some(parser.into())
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub struct Ident(pub String);

impl Parse for Ident {
    fn parse(parser: &mut Parser) -> Option<Self> {
        let Token::Ident(ident) = parser.take_token()? else {
            return None;
        };
        Some(Self(ident))
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Type {
    Identifier(Ident),
}

impl Parse for Type {
    fn parse(parser: &mut Parser) -> Option<Self> {
        Some(Self::Identifier(parser.parse()?))
    }
}

impl From<Type> for Ident {
    fn from(value: Type) -> Self {
        match value {
            Type::Identifier(ident) => ident,
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Statement {
    Expression(Expression),
    Let(Ident, Expression),
}

impl Parse for Statement {
    fn parse(parser: &mut Parser) -> Option<Self> {
        if parser.take_token_if(&Token::Let).is_some() {
            let name = parser.parse()?;
            parser.take_token_if(&Token::Assignment)?;
            Some(Self::Let(name, parser.parse()?))
        } else {
            parser.parse().map(Self::Expression)
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
        pub use std::{collections::HashMap, iter::Peekable};
        pub use tokenizer::{Token, Tokenizer};
    }
}

pub mod prelude {
    pub use crate::{
        expression::{self, prelude::*},
        parse_file,
        parser::{self, Parser},
        scope::{self, Scope},
        top_level::{self, prelude::*},
        Expression, Ident, Parse, Statement, Type,
    };
}
