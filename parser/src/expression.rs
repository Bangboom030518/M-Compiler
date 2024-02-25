use macros::Spanned;
use tokenizer::{Span, TokenType};

use crate::{internal::prelude::*, Error};

pub mod binary;
pub mod control_flow;

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum UnaryOperator {
    Minus,
    Bang,
    Return,
}

impl Parse for UnaryOperator {
    fn parse(parser: &mut Parser) -> Result<Self, Error> {
        match parser.take_token()? {
            Token::Minus => Some(Self::Minus),
            Token::Bang => Some(Self::Bang),
            Token::Return => Some(Self::Return),
            _ => None,
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum LiteralKind {
    String(String),
    Integer(u128),
    Float(f64),
    Char(char),
}

#[derive(Clone, Debug, Spanned)]
pub struct Literal {
    pub kind: LiteralKind,
    #[span]
    span: tokenizer::Span,
}

impl Parse for Literal {
    fn parse(parser: &mut Parser) -> Result<Self, Error> {
        let (kind, span) = parser.take_token_if(TokenType::String).map(|token| match token.token {
            Token::String(string) => (LiteralKind::String(string), token.span),
            _ => unreachable!(),
        }).or_else(|_| parser.take_token_if(TokenType::Integer).map(|token| match token.token {
            Token::Integer(integer) => (LiteralKind::Integer(integer), token.span),
            _ => unreachable!(),
        })).or_else(|_| parser.take_token_if(TokenType::Float).map(|token| match token.token {
            Token::Float(float) => (LiteralKind::Float(float), token.span),
            _ => unreachable!(),
        })).or_else(|_| parser.take_token_if(TokenType::Char).map(|token| match token.token {
            Token::Char(char) => (LiteralKind::Char(char), token.span),
            _ => unreachable!(),
        }))?;
        Ok(Self {
            kind, span
        })
    }
}

#[derive(Debug, Clone, Spanned)]
pub struct Call {
    pub callable: Box<Expression>,
    pub type_arguments: Vec<crate::Type>,
    pub arguments: Vec<Expression>,
    #[span]
    span: tokenizer::Span,
}

impl Parse for Call {
    fn parse(parser: &mut Parser) -> Result<Self, Error> {
        let callable = Expression::parse_nonpostfix_term(parser)?;
        let type_arguments = if parser.take_token_if(TokenType::LessThan).is_some() {
            let type_arguments = parser.parse_csv();
            parser.take_token_if(TokenType::GreaterThan)?;
            type_arguments
        } else {
            Vec::new()
        };
        parser.take_token_if(TokenType::OpenParen)?;
        let arguments = parser.parse_csv();

        let start = callable.span().start;
        let end = parser.take_token_if(TokenType::CloseParen)?.span.end;

        Ok(Self {
            callable: Box::new(callable),
            type_arguments,
            arguments,
            span: start..end
        })
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum CmpOperator {
    Lt,
    Gt,
    Lte,
    Gte,
    Eq,
    Ne,
}

impl CmpOperator {
    #[must_use]
    pub const fn signed_intcc(self) -> cranelift::prelude::IntCC {
        use cranelift::prelude::IntCC;
        match self {
            Self::Eq => IntCC::Equal,
            Self::Ne => IntCC::NotEqual,
            Self::Gt => IntCC::SignedGreaterThan,
            Self::Gte => IntCC::SignedGreaterThanOrEqual,
            Self::Lt => IntCC::SignedLessThan,
            Self::Lte => IntCC::SignedLessThanOrEqual,
        }
    }

    #[must_use]
    pub const fn unsigned_intcc(self) -> cranelift::prelude::IntCC {
        use cranelift::prelude::IntCC;
        match self {
            Self::Eq => IntCC::Equal,
            Self::Ne => IntCC::NotEqual,
            Self::Gt => IntCC::UnsignedGreaterThan,
            Self::Gte => IntCC::UnsignedGreaterThanOrEqual,
            Self::Lt => IntCC::UnsignedLessThan,
            Self::Lte => IntCC::UnsignedLessThanOrEqual,
        }
    }

    #[must_use]
    pub fn floatcc(self) -> cranelift::prelude::FloatCC {
        use cranelift::prelude::FloatCC;
        match self {
            Self::Eq => FloatCC::Equal,
            Self::Ne => FloatCC::NotEqual,
            Self::Gt => todo!(),
            Self::Gte => todo!(),
            Self::Lt => todo!(),
            Self::Lte => todo!(),
        }
    }
}
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum IntrinsicOperator {
    Cmp(CmpOperator),
    Add,
    Sub,
}

impl IntrinsicOperator {
    fn from_str(string: &str) -> Result<Self, Error> {
        let operator = match string {
            "add" => Self::Add,
            "sub" => Self::Sub,
            "lt" => Self::Cmp(CmpOperator::Lt),
            "gt" => Self::Cmp(CmpOperator::Gt),
            "lte" => Self::Cmp(CmpOperator::Lte),
            "gte" => Self::Cmp(CmpOperator::Gte),
            "eq" => Self::Cmp(CmpOperator::Eq),
            "ne" => Self::Cmp(CmpOperator::Ne),
            _ => return None,
        };
        Some(operator)
    }
}

#[derive(Debug, Clone)]
pub enum IntrinsicCall {
    AssertType(Box<Expression>, Type),
    MutablePointer(Box<Expression>),
    Deref(Box<Expression>),
    Binary(Box<Expression>, Box<Expression>, IntrinsicOperator),
}

impl Parse for IntrinsicCall {
    fn parse(parser: &mut Parser) -> Result<Self, Error> {
        parser.take_token_if(TokenType::At)?;
        let ident = parser.take_token_if(TokenType::Ident).map(|token| match token.token {
            Token::Ident(ident) => ident,
            _ => unreachable!(),
        })?;

        parser.take_token_if(TokenType::OpenParen)?;
        let value = match ident.as_str() {
            "assert_type" => {
                let expression = Box::new(parser.parse()?);
                parser.take_token_if(TokenType::Comma)?;
                Self::AssertType(expression, parser.parse()?)
            },
            "mutable_pointer" => {
                Self::MutablePointer(Box::new(parser.parse()?))
            },
            "deref" => {
                Self::Deref(Box::new(parser.parse()?))
            },
            
            token if let Ok(op) = IntrinsicOperator::from_str(token) => {
                let left = Box::new(parser.parse()?);
                parser.take_token_if(TokenType::Comma)?;
                let right = Box::new(parser.parse()?);

                Self::Binary(left, right, op)
            },
            _ => return None,
        };

        parser.take_token_if(TokenType::CloseParen)?;
        Ok(value)
    }
}

#[derive(Debug, Clone, Spanned)]
pub struct Constructor {
    pub r#type: Type,
    pub fields: Vec<(Ident, Box<Expression>)>,
    #[span]
    span: tokenizer::Span
}

impl Parse for Constructor {
    fn parse(parser: &mut Parser) -> Result<Self, Error> {
        let r#type = parser.parse()?;
        let mut fields = Vec::new();
        
        loop {
            let field = parser.parse::<Ident>()?;
            parser.take_token_if(TokenType::Assignment)?;
            fields.push((field, Box::new(parser.parse()?)));
            if parser.take_token_if(TokenType::Comma).is_err() {
                parser.peek_token_if(TokenType::End)?;
                break;
            } else if parser.peek_token_if(TokenType::End).is_ok() {
                break;
            }
        }
        let start = r#type.span().start;
        let end = parser.take_token_if(TokenType::End)?.span.end;
        Ok(Self { r#type, fields, span: start..end })
    }
}

#[derive(Debug, Clone, Spanned)]
pub enum Expression {
    Ident(Ident),
    IntrinsicCall(IntrinsicCall),
    Literal(Literal),
    Binary(binary::Expression),
    UnaryPrefix(UnaryOperator, Box<Self>),
    Call(Call),
    If(If),
    Return(Box<Expression>),
    Constructor(Constructor),
    FieldAccess(Box<Expression>, Ident),
}

// TODO:
#[cfg(ignore)]
pub enum Path {
    Ident(Ident),
    Generic(Vec<Ident>, ScopeAccessor),
    NamespaceOrFieldAccess { parent: Ident, child: Ident },
}

impl Expression {
    fn parse_term(parser: &mut Parser) -> Result<Self, Error> {
        if parser.take_token_if(TokenType::OpenParen).is_some() {
            let expression = parser.parse()?;
            parser.take_token_if(TokenType::CloseParen)?;
            return Some(expression);
        }
        // TODO: parser.scope(|parser| {})
        parser
            .parse::<Call>()
            .map(Self::Call)
            .or_else(|_| {
                parser.scope(|parser| {
                    // TODO: may not be ident
                    let expr = Box::new(parser.parse().map(Self::Ident)?);
                    parser.take_token_if(TokenType::Dot)?;
                    Some(Self::FieldAccess(expr, parser.parse()?))
                })
            })
            .or_else(|_| parser.parse().map(Self::Constructor))
            .or_else(|_| {
                parser.scope(|parser| {
                    parser.take_token_if(TokenType::Return)?;
                    Some(Self::Return(Box::new(parser.parse()?)))
                })
            })
            .or_else(|| parser.parse().map(Self::IntrinsicCall))
            .or_else(|| Self::parse_nonpostfix_term(parser))
    }

    fn parse_nonpostfix_term(parser: &mut Parser) -> Result<Self, Error> {
        if parser.take_token_if(TokenType::OpenParen).is_some() {
            let expression = parser.parse()?;
            parser.take_token_if(TokenType::CloseParen)?;
            return Some(expression);
        }

        parser
            .parse()
            .map(Self::Literal)
            .or_else(|| parser.parse().map(Self::If))
            .or_else(|| parser.parse().map(Self::Ident))
            .or_else(|| {
                Some(Self::UnaryPrefix(
                    parser.parse()?,
                    Box::new(parser.parse()?),
                ))
            })
    }
}

impl Parse for Expression {
    fn parse(parser: &mut Parser) -> Result<Self, Error> {
        Ok(parser.parse::<binary::Terms>()?.into())
    }
}

pub mod prelude {
    pub use super::control_flow::If;
    pub use super::{binary, Literal};
}
