use crate::internal::prelude::*;

pub mod binary;
pub mod control_flow;

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum UnaryOperator {
    Minus,
    Bang,
    Return,
}

impl Parse for UnaryOperator {
    fn parse(parser: &mut Parser) -> Option<Self> {
        match parser.take_token()? {
            Token::Minus => Some(Self::Minus),
            Token::Bang => Some(Self::Bang),
            Token::Return => Some(Self::Return),
            _ => None,
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Literal {
    String(String),
    Integer(u128),
    Float(f64),
    Char(char),
}

impl Parse for Literal {
    fn parse(parser: &mut Parser) -> Option<Self> {
        match parser.take_token()? {
            Token::String(string) => Some(Self::String(string)),
            Token::Integer(integer) => Some(Self::Integer(integer)),
            Token::Float(float) => Some(Self::Float(float)),
            Token::Char(char) => Some(Self::Char(char)),
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
    fn parse(parser: &mut Parser) -> Option<Self> {
        let callable = Expression::parse_nonpostfix_term(parser)?;

        let type_arguments = if parser.take_token_if(&Token::LessThan).is_some() {
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
pub enum IntrinsicCall {
    IAdd(Box<Expression>, Box<Expression>),
}

impl Parse for IntrinsicCall {
    fn parse(parser: &mut Parser) -> Option<Self> {
        parser.take_token_if(&Token::At)?;
        let Token::Ident(ident) = parser.take_token()? else {
            return None;
        };

        parser.take_token_if(&Token::OpenParen)?;
        let value = match ident.as_str() {
            "iadd" => {
                let left = Box::new(parser.parse()?);
                parser.take_token_if(&Token::Comma)?;
                let right = Box::new(parser.parse()?);

                Self::IAdd(left, right)
            }
            _ => return None,
        };

        parser.take_token_if(&Token::CloseParen)?;
        Some(value)
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Expression {
    Identifier(Ident),
    IntrinsicCall(IntrinsicCall),
    Literal(Literal),
    Binary(binary::Expression),
    UnaryPrefix(UnaryOperator, Box<Self>),
    Call(Call),
    If(If),
    Return(Box<Expression>),
}

// TODO:
#[cfg(ignore)]
pub enum Path {
    Ident(Ident),
    Generic(Vec<Ident>, ScopeAccessor),
    NamespaceOrFieldAccess { parent: Ident, child: Ident },
}

impl Expression {
    fn parse_term(parser: &mut Parser) -> Option<Self> {
        if parser.take_token_if(&Token::OpenParen).is_some() {
            let expression = parser.parse()?;
            parser.take_token_if(&Token::CloseParen)?;
            return Some(expression);
        }

        parser
            .parse::<Call>()
            .map(Self::Call)
            .or_else(|| {
                parser.take_token_if(&Token::Return)?;
                Some(Self::Return(Box::new(parser.parse()?)))
            })
            .or_else(|| parser.parse::<IntrinsicCall>().map(Self::IntrinsicCall))
            .or_else(|| Self::parse_nonpostfix_term(parser))
    }

    fn parse_nonpostfix_term(parser: &mut Parser) -> Option<Self> {
        if parser.take_token_if(&Token::OpenParen).is_some() {
            let expression = parser.parse()?;
            parser.take_token_if(&Token::CloseParen)?;
            return Some(expression);
        }

        parser
            .parse::<Literal>()
            .map(Self::Literal)
            .or_else(|| parser.parse::<If>().map(Self::If))
            .or_else(|| parser.parse::<Ident>().map(Self::Identifier))
            .or_else(|| {
                Some(Self::UnaryPrefix(
                    parser.parse()?,
                    Box::new(parser.parse()?),
                ))
            })
    }
}

impl Parse for Expression {
    fn parse(parser: &mut Parser) -> Option<Self> {
        Some(parser.parse::<binary::Terms>()?.into())
    }
}

pub mod prelude {
    pub use super::binary;
    pub use super::control_flow::If;
    pub use super::Literal;
}
