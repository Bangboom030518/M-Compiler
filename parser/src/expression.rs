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

#[derive(Debug, Clone, PartialEq)]
pub struct Call {
    pub callable: Box<Expression>,
    pub type_arguments: Vec<crate::Type>,
    pub arguments: Vec<Expression>,
}

impl Parse for Call {
    fn parse<'a>(parser: &mut Parser<'a>) -> Option<Self> {
        let callable = Expression::parse_nonpostfix_term(parser)?;

        let type_arguments = 
        if parser.take_token_if(&Token::LessThan).is_some() {
            let type_arguments = parser.parse_csv();
            parser.take_token_if(&Token::GreaterThan)?;
            type_arguments
        } else {
            Vec::new()
        };
        parser.take_token_if(&Token::OpenParen)?;
        let arguments = parser.parse_csv();
        parser.take_token_if(&Token::CloseParen)?;

        Some(Self {
            callable: Box::new(callable),
            type_arguments,
            arguments,
        })
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Expression {
    Identifier(Identifier),
    Literal(Literal),
    Binary(binary::Expression),
    UnaryPrefix(UnaryOperator, Box<Self>),
    Call(Call),
}

impl Expression {
    fn parse_term<'a>(parser: &mut Parser<'a>) -> Option<Self> {
        if parser.take_token_if(&Token::OpenParen).is_some() {
            let expression = parser.parse()?;
            parser.take_token_if(&Token::CloseParen)?;
            return Some(expression);
        }

        parser
            .parse::<Call>()
            .map(Self::Call)
            .or_else(|| Self::parse_nonpostfix_term(parser))
    }

    // TODO: rename
    fn parse_nonpostfix_term<'a>(parser: &mut Parser<'a>) -> Option<Self> {
        if parser.take_token_if(&Token::OpenParen).is_some() {
            let expression = parser.parse()?;
            parser.take_token_if(&Token::CloseParen)?;
            return Some(expression);
        }

        parser
            .parse::<Literal>()
            .map(Self::Literal)
            .or_else(|| parser.parse::<Identifier>().map(Self::Identifier))
            .or_else(|| {
                Some(Self::UnaryPrefix(
                    parser.parse()?,
                    Box::new(parser.parse()?),
                ))
            })
    }
}

impl Parse for Expression {
    fn parse<'a>(parser: &mut Parser<'a>) -> Option<Self> {
        Some(parser.parse::<binary::Terms>()?.into())
    }
}
