use crate::parser::{Error, Parser};
use crate::{Ident, Parse};
use tokenizer::{AsSpanned, Spanned, TokenType};

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Parameter {
    pub name: Spanned<Ident>,
    pub kind: Kind,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Kind {
    Type,
    Length,
}

impl Parse for Parameter {
    fn parse(parser: &mut Parser) -> Result<Spanned<Self>, Error> {
        let value = if let Ok(at) = parser.take_token_if(TokenType::At) {
            if parser.take_ident()?.value != "length" {
                return Err(parser.unexpected_ident(vec!["length"]));
            }
            let name = parser.parse()?;
            let end = name.end();
            Self {
                name,
                kind: Kind::Length,
            }
            .spanned(at.start()..end)
        } else {
            let name = parser.parse()?;
            let span = name.span.clone();
            Self {
                name,
                kind: Kind::Type,
            }
            .spanned(span)
        };
        Ok(value)
    }
}

#[derive(PartialEq, Eq, Debug, Default, Clone)]
pub struct Parameters {
    pub generics: Vec<Spanned<Parameter>>,
}

impl Parse for Parameters {
    fn parse(parser: &mut Parser) -> Result<Spanned<Self>, Error> {
        let Ok(open) = parser.take_token_if(TokenType::OpenSquareParen) else {
            return Ok(Self {
                generics: Vec::new(),
            }
            .spanned(parser.empty_span()));
        };
        let generics = parser.parse_csv();
        Ok(Self { generics }
            .spanned(open.start()..parser.take_token_if(TokenType::CloseSquareParen)?.end()))
    }
}
