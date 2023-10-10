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
pub fn parse_file(input: &str) -> Option<Vec<top_level::Declaration>> {
    let mut parser = Parser::from(Tokenizer::from(input));
    let scope_id = parser.create_scope();
    let scope = parser.get_scope(scope_id);
    while let Some(declaration) = parser.parse_line::<top_level::Declaration>() {
        scope
            .declarations
            .insert(declaration.name, declaration.kind);
    }
    parser.exit_scope();

    Some(todo!())
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub struct Identifier(String);

impl Parse for Identifier {
    fn parse(parser: &mut Parser) -> Option<Self> {
        let Token::Ident(ident) = parser.take_token()? else {
            return None;
        };
        Some(Self(ident))
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Type {
    Identifier(Identifier),
}

impl Parse for Type {
    fn parse(parser: &mut Parser) -> Option<Self> {
        Some(Self::Identifier(parser.parse()?))
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Statement {
    Expression(Expression),
    Let(Identifier, Expression),
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
    let cache = &mut scope::Cache::new();
    let mut parser = Parser::from(Tokenizer::from(source));
    let r#let = parser.parse::<Statement>().unwrap();
    assert_eq!(
        r#let,
        Statement::Let(
            Identifier(String::from("a")),
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
        Expression, Identifier, Parse, Statement, Type,
    };
}
