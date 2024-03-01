use std::str::FromStr;

use crate::internal::prelude::*;
use crate::Error;
use macros::Spanned;
use tokenizer::TokenType;

pub mod binary;
pub mod control_flow;

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum UnaryOperatorKind {
    Minus,
    Bang,
    Return,
}

#[derive(PartialEq, Eq, Debug, Clone, Spanned)]
pub struct UnaryOperator {
    kind: UnaryOperatorKind,
    #[span]
    span: tokenizer::Span,
}

impl Parse for UnaryOperator {
    fn parse(parser: &mut Parser) -> Result<Self, Error> {
        parser
            .take_token_if(TokenType::Minus)
            .map(|token| Self {
                kind: UnaryOperatorKind::Minus,
                span: token.span,
            })
            .or_else(|_| {
                parser.take_token_if(TokenType::Bang).map(|token| Self {
                    kind: UnaryOperatorKind::Bang,
                    span: token.span,
                })
            })
            .or_else(|_| {
                parser.take_token_if(TokenType::Return).map(|token| Self {
                    kind: UnaryOperatorKind::Return,
                    span: token.span,
                })
            })
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum LiteralKind {
    String(String),
    Integer(u128),
    Float(f64),
    Char(char),
}

#[derive(Clone, Debug, Spanned, PartialEq)]
pub struct Literal {
    pub kind: LiteralKind,
    #[span]
    span: tokenizer::Span,
}

impl Parse for Literal {
    fn parse(parser: &mut Parser) -> Result<Self, Error> {
        parser
            .take_string()
            .map(|(span, value)| (span, LiteralKind::String(value)))
            .or_else(|_| {
                parser
                    .take_integer()
                    .map(|(span, value)| (span, LiteralKind::Integer(value)))
            })
            .or_else(|_| {
                parser
                    .take_float()
                    .map(|(span, value)| (span, LiteralKind::Float(value)))
            })
            .or_else(|_| {
                parser
                    .take_char()
                    .map(|(span, value)| (span, LiteralKind::Float(value)))
            })
            .map(|(span, kind)| Self { kind, span })
    }
}

#[derive(Debug, Clone, Spanned, PartialEq)]
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
        let type_arguments = if parser.take_token_if(TokenType::LessThan).is_ok() {
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
            span: start..end,
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
            _ => todo!("nice error"),
        };
        Ok(operator)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum IntrinsicCallKind {
    AssertType(Box<Expression>, Type),
    MutablePointer(Box<Expression>),
    Deref(Box<Expression>),
    Binary(Box<Expression>, Box<Expression>, IntrinsicOperator),
}

#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct IntrinsicCall {
    kind: IntrinsicCallKind,
    #[span]
    span: tokenizer::Span,
}

impl Parse for IntrinsicCall {
    fn parse(parser: &mut Parser) -> Result<Self, Error> {
        let start = parser.take_token_if(TokenType::At)?.start();
        let ident = parser.take_token_if(TokenType::Ident).map(|token| {
            (
                match token.token {
                    Token::Ident(ident) => ident,
                    _ => unreachable!(),
                },
                token.span,
            )
        })?;

        parser.take_token_if(TokenType::OpenParen)?;
        let kind = match ident.as_str() {
            "assert_type" => {
                let expression = Box::new(parser.parse()?);
                parser.take_token_if(TokenType::Comma)?;
                IntrinsicCallKind::AssertType(expression, parser.parse()?)
            }
            "mutable_pointer" => IntrinsicCallKind::MutablePointer(Box::new(parser.parse()?)),
            "deref" => IntrinsicCallKind::Deref(Box::new(parser.parse()?)),

            token if let Ok(operator) = token.parse() => {
                let left = Box::new(parser.parse()?);
                parser.take_token_if(TokenType::Comma)?;
                let right = Box::new(parser.parse()?);

                IntrinsicCallKind::Binary(left, right, operator)
            }
            _ => return None,
        };

        let end = parser.take_token_if(TokenType::CloseParen)?.end();
        Ok(Self {
            kind,
            span: start..end,
        })
    }
}

#[derive(Debug, Clone, Spanned)]
pub struct Constructor {
    pub r#type: Type,
    pub fields: Vec<(Ident, Box<Expression>)>,
    #[span]
    span: tokenizer::Span,
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
        Ok(Self {
            r#type,
            fields,
            span: start..end,
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryPrefix {
    operator: UnaryOperator,
    expression: Expression,
}

impl Parse for UnaryPrefix {
    fn parse(parser: &mut Parser) -> Result<Self, Error> {
        Ok(Self {
            operator: parser.parse()?,
            expression: parser.parse()?,
        })
    }
}

impl Spanned for UnaryPrefix {
    fn span(&self) -> tokenizer::Span {
        self.operator.start()..self.expression.end()
    }
}

#[derive(Debug, Clone, PartialEq, Spanned)]
struct Return {
    expression: Expression,
    #[span]
    span: tokenizer::Span,
}

impl Parse for Return {
    fn parse(parser: &mut Parser) -> Result<Self, Error> {
        let start = parser.take_token_if(TokenType::Return)?.start;
        let expression = Box::new(parser.parse()?);
        Some(Self {
            span: start..expression.end(),
            expression,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct FieldAccess {
    #[span(start)]
    expression: Expression,
    #[span(end)]
    ident: Ident,
}

impl Parse for FieldAccess {
    fn parse(parser: &mut Parser) -> Result<Self, Error> {
        let expression = Box::new(parser.parse().map(Self::Ident)?);
        parser.take_token_if(TokenType::Dot)?;
        Ok(Self {
            expression,
            ident: parser.parse()?,
        })
    }
}

#[derive(Debug, Clone, Spanned, PartialEq)]
pub enum Expression {
    Ident(Ident),
    IntrinsicCall(IntrinsicCall),
    Literal(Literal),
    Binary(binary::Expression),
    UnaryPrefix(Box<UnaryPrefix>),
    Call(Call),
    If(If),
    Return(Box<Return>),
    Constructor(Constructor),
    FieldAccess(Box<FieldAccess>),
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
        if parser.take_token_if(TokenType::OpenParen).is_ok() {
            let expression = parser.parse()?;
            parser.take_token_if(TokenType::CloseParen)?;
            return Ok(expression);
        }
        // TODO: parser.scope(|parser| {})
        parser
            .parse::<Call>()
            .map(Self::Call)
            .or_else(|_| parser.parse().map(Box::new).map(Self::FieldAccess))
            .or_else(|_| parser.parse().map(Self::Constructor))
            .or_else(|_| parser.parse().map(Box::new).map(Self::Return))
            .or_else(|_| parser.parse().map(Self::IntrinsicCall))
            .or_else(|_| Self::parse_nonpostfix_term(parser))
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
            .or_else(|| parser.parse().map(Self::UnaryPrefix))
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
