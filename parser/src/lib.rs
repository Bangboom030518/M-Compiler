#![warn(
    clippy::pedantic,
    clippy::nursery,
    clippy::unwrap_used,
    clippy::unreachable
)]
#![feature(assert_matches, iter_collect_into, if_let_guard)]

pub use expression::{Expression, Literal};
use parser::{Error, Parser};
use std::collections::HashMap;
use std::fmt::Display;
use tokenizer::{
    AsSpanned, Spanned, SpannedResultExt, Token, TokenType, TokenTypeBitFields, Tokenizer,
};
pub use top_level::{Declaration, ExternFunction, Function, Struct, Union};

pub mod expression;
pub mod parser;
pub mod top_level;

trait Parse {
    fn parse(parser: &mut Parser) -> Result<Spanned<Self>, Error>
    where
        Self: Sized;
}

#[derive(Clone, Debug, thiserror::Error)]
#[error("expected one of '{expected}', found '{:?}'", found.value)]
pub struct ParseError {
    expected: TokenTypeBitFields,
    found: Spanned<Token>,
}

/// # Errors
/// if the input is unparseable
pub fn parse_file(input: &str) -> Result<HashMap<String, Declaration>, ParseError> {
    let mut parser = Parser::from(Tokenizer::from(input));
    let mut declarations = HashMap::new();
    loop {
        match parser.parse::<top_level::Declaration>() {
            Ok(declaration) => {
                declarations.insert(declaration.value.name().to_string(), declaration.value);
                if parser.peek_token_if(TokenType::Eoi).is_ok() {
                    return Ok(declarations);
                };
            }
            Err(error) => return Err(parser.parse_error(error)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident(pub String);

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl AsRef<str> for Ident {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl Parse for Ident {
    fn parse(parser: &mut Parser) -> Result<Spanned<Self>, Error> {
        parser.take_ident().map_spanned(Ident)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GenericArgument {
    Type(Type),
    Literal(u128),
}

impl Parse for GenericArgument {
    fn parse(parser: &mut Parser) -> Result<Spanned<Self>, Error> {
        parser
            .parse()
            .map_spanned(Self::Type)
            .or_else(|_| parser.take_integer().map_spanned(Self::Literal))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type {
    pub name: Spanned<Ident>,
    pub generics: Spanned<Vec<Spanned<GenericArgument>>>,
}

impl Parse for Type {
    fn parse(parser: &mut Parser) -> Result<Spanned<Self>, Error> {
        let name = parser.parse()?;
        let (generics, span) =
            if let Ok(open) = parser.take_token_if(TokenType::OpenSquareParen) {
                let generics = parser.parse_csv();
                let close = parser.take_token_if(TokenType::CloseSquareParen)?;
                (
                    generics.spanned(open.start()..close.end()),
                    name.start()..close.end(),
                )
            } else {
                (Vec::new().spanned(parser.empty_span()), name.span.clone())
            };

        Ok(Self { name, generics }.spanned(span))
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
        let span = start..expression.end();
        Ok(Self { ident, expression }.spanned(span))
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
        let span = left.start()..right.end();
        Ok(Self { left, right }.spanned(span))
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
