use crate::internal::prelude::*;

mod binary;

#[derive(PartialEq, Debug, Clone)]
pub enum UnaryOperator {
    Minus,
    Bang,
}

impl Parse for UnaryOperator {
    fn parse<'a>(parser: &mut Parser<'a>) -> Option<Self> {
        match parser.take_token()? {
            Token::Minus => Some(Self::Minus),
            Token::Bang => Some(Self::Bang),
            _ => None,
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Literal {
    String(String),
    Integer(u64),
    Float(f64),
}

impl Parse for Literal {
    fn parse<'a>(parser: &mut Parser<'a>) -> Option<Self> {
        match parser.take_token()? {
            Token::String(string) => Some(Self::String(string)),
            Token::Integer(integer) => Some(Self::Integer(integer)),
            Token::Float(float) => Some(Self::Float(float)),
            _ => None,
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Expression {
    Identifier(Identifier),
    Literal(Literal),
    Binary(binary::Expression),
    Unary(UnaryOperator, Box<Self>),
}

impl Expression {
    fn parse_term<'a>(parser: &mut Parser<'a>) -> Option<Self> {
        if parser.next_token_is(&Token::OpenParen) {
            let expression = parser.parse()?;
            parser.next_token_is(&Token::CloseParen).then_some(())?;
            return Some(expression);
        }

        None.or_else(|| parser.parse::<Literal>().map(Self::Literal))
            .or_else(|| parser.parse::<Identifier>().map(Self::Identifier))
            .or_else(|| Some(Self::Unary(parser.parse()?, Box::new(parser.parse()?))))
    }
}

impl Parse for Expression {
    fn parse<'a>(parser: &mut Parser<'a>) -> Option<Self> {
        Some(parser.parse::<binary::Terms>()?.into())
    }
}
