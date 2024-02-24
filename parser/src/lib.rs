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
use tokenizer::{SpannedToken, TokenType};

pub mod expression;
pub mod parser;
pub mod scope;
pub mod top_level;

#[derive(Clone, Debug, thiserror::Error)]
#[error("Unexpected token '{found:?}'. Expected one of '{expected:?}'")]
pub struct Error<'a> {
    pub expected: &'a HashSet<TokenType>,
    pub found: &'a SpannedToken,
}

pub trait Parse {
    fn parse(parser: &mut Parser) -> Result<Self, Error>
    where
        Self: Sized;

    fn span(&self) -> tokenizer::Span;
}

#[must_use]
pub fn parse_file(input: &str) -> Result<scope::File, Error> {
    let mut parser = Parser::from(Tokenizer::from(input));
    let declarations = &mut parser.get_scope(parser.scope).declarations;
    loop {
        let declaration = parser.parse::<top_level::Declaration>()?;
        declarations.insert(declaration.name.ident, declaration.kind);
        if parser.peek_token_if(TokenType::Eoi).is_ok() {
            return Ok(parser.into());
        };
    }
}

#[derive(Debug, Clone)]
pub struct Ident {
    pub ident: String,
    pub span: tokenizer::Span,
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
    fn parse(parser: &mut Parser) -> Result<Self, Error> {
        parser
            .take_token_if(TokenType::Ident)
            .map(|token| match token.token {
                Token::Ident(ident) => Self {
                    ident,
                    span: token.span,
                },
                _ => unreachable!(),
            })
    }

    fn span(&self) -> tokenizer::Span {
        self.span
    }
}

#[derive(Debug, Clone)]
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
    fn parse(parser: &mut Parser) -> Result<Self, Error> {
        parser.parse().map(Self::Ident)
    }

    fn span(&self) -> tokenizer::Span {
        match self {
            Self::Ident(ident) => ident.span(),
        }
    }
}

impl From<Type> for Ident {
    fn from(value: Type) -> Self {
        match value {
            Type::Ident(ident) => ident,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Let {
    pub ident: Ident,
    pub expression: Expression,
    span: tokenizer::Span,
}

impl Parse for Let {
    fn parse(parser: &mut Parser) -> Result<Self, Error> {
        let start = parser.take_token_if(TokenType::Let)?.span.start;
        let ident = parser.parse()?;
        parser.take_token_if(TokenType::Assignment)?;
        let expression = parser.parse()?;
        Ok(Self {
            ident,
            expression,
            span: start..expression.span().end,
        })
    }

    fn span(&self) -> tokenizer::Span {
        self.span
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(Expression),
    Let(Let),
    // TODO: accessors!
    Assignment(Assignment),
}

#[derive(PartialEq, Debug, Clone)]
pub struct Assignment {
    pub left: Expression,
    pub right: Expression,
}

impl Parse for Assignment {
    fn parse(parser: &mut Parser) -> Result<Self, Error> {
        let left = parser.parse()?;
        parser.take_token_if(TokenType::Assignment)?;
        Ok(Self {
            left,
            right: parser.parse()?,
        })
    }

    fn span(&self) -> tokenizer::Span {
        self.left.span().start..self.right.span().end
    }
}

impl Parse for Statement {
    fn parse(parser: &mut Parser) -> Result<Self, Error> {
        parser
            .parse()
            .map(Self::Let)
            .or_else(|_| parser.parse().map(Self::Assignment))
            .or_else(|_| parser.parse().map(Self::Expression))
    }

    fn span(&self) -> tokenizer::Span {
        match self {
            Self::Assignment(assignment) => assignment.span(),
            Self::Expression(expression) => expression.span(),
            Self::Let(statement) => statement.span(),
        }
    }
}

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
