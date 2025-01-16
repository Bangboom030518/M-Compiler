use crate::parser::Parser;
use crate::{Error, Ident, Parse, Type};
use control_flow::If;
use tokenizer::{AsSpanned, Spanned, SpannedResultExt, TokenType};

pub mod binary;
pub mod control_flow;

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum UnaryOperator {
    Minus,
    Bang,
}

impl Parse for UnaryOperator {
    fn parse(parser: &mut Parser) -> Result<Spanned<Self>, Error> {
        parser
            .take_token_if(TokenType::Minus)
            .with_spanned(Self::Minus)
            .or_else(|_| {
                parser
                    .take_token_if(TokenType::Bang)
                    .with_spanned(Self::Bang)
            })
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Literal {
    String(String),
    Integer(u64),
    Float(f64),
    Char(char),
}

impl Parse for Literal {
    fn parse(parser: &mut Parser) -> Result<Spanned<Self>, Error> {
        parser
            .take_string()
            .map_spanned(Self::String)
            .or_else(|_| parser.take_integer().map_spanned(Self::Integer))
            .or_else(|_| parser.take_float().map_spanned(Self::Float))
            .or_else(|_| parser.take_char().map_spanned(Self::Char))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Call {
    pub callable: Box<Spanned<Expression>>,
    pub type_arguments: Vec<Spanned<crate::Type>>,
    pub arguments: Vec<Spanned<Expression>>,
}

impl Call {
    fn parse(parser: &mut Parser, mut allow: TermKindBitFields) -> Result<Spanned<Self>, Error> {
        allow.remove(TermKind::Call);
        let callable = Expression::parse_term(parser, allow)?;
        let type_arguments = if parser.take_token_if(TokenType::LessThan).is_ok() {
            let type_arguments = parser.parse_csv();
            parser.take_token_if(TokenType::GreaterThan)?;
            type_arguments
        } else {
            Vec::new()
        };

        parser.take_token_if(TokenType::OpenParen)?;

        let arguments = parser.parse_csv();

        let start = callable.start();
        let end = parser.take_token_if(TokenType::CloseParen)?.span.end;

        Ok(Self {
            callable: Box::new(callable),
            type_arguments,
            arguments,
        }
        .spanned(start..end))
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
    pub const fn floatcc(self) -> cranelift::prelude::FloatCC {
        use cranelift::prelude::FloatCC;
        match self {
            Self::Eq => FloatCC::Equal,
            Self::Ne => FloatCC::NotEqual,
            Self::Gt => FloatCC::GreaterThan,
            Self::Gte => FloatCC::GreaterThanOrEqual,
            Self::Lt => FloatCC::LessThan,
            Self::Lte => FloatCC::LessThanOrEqual,
        }
    }
}
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum IntrinsicOperator {
    Cmp(CmpOperator),
    Add,
    Sub,
    Mul,
    Div,
}

impl IntrinsicOperator {
    const OPERATORS: &'static [&'static str] =
        &["add", "sub", "lt", "gt", "lte", "gte", "eq", "ne"];
    fn from_str(string: &str, parser: &Parser) -> Result<Self, Error> {
        let operator = match string {
            "add" => Self::Add,
            "sub" => Self::Sub,
            "mul" => Self::Mul,
            "div" => Self::Div,
            "lt" => Self::Cmp(CmpOperator::Lt),
            "gt" => Self::Cmp(CmpOperator::Gt),
            "lte" => Self::Cmp(CmpOperator::Lte),
            "gte" => Self::Cmp(CmpOperator::Gte),
            "eq" => Self::Cmp(CmpOperator::Eq),
            "ne" => Self::Cmp(CmpOperator::Ne),
            _ => return Err(parser.unexpected_ident(Self::OPERATORS.to_vec())),
        };
        Ok(operator)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum IntrinsicCall {
    AssertType(Box<Spanned<Expression>>, Spanned<Type>),
    Addr(Box<Spanned<Expression>>),
    SizeOf(Spanned<Type>),
    Load(Box<Spanned<Expression>>),
    Store {
        pointer: Box<Spanned<Expression>>,
        expression: Box<Spanned<Expression>>,
    },
    Binary(
        Box<Spanned<Expression>>,
        Box<Spanned<Expression>>,
        IntrinsicOperator,
    ),
    True,
    False,
}

impl IntrinsicCall {
    const IDENTS: &'static [&'static str] = &[
        "assert_type",
        "addr",
        "load",
        "store",
        "add",
        "sub",
        "lt",
        "gt",
        "lte",
        "gte",
        "eq",
        "ne",
        "true",
        "false",
        "sizeof",
    ];
}

impl Parse for IntrinsicCall {
    fn parse(parser: &mut Parser) -> Result<Spanned<Self>, Error> {
        let start = parser.take_token_if(TokenType::At)?.start();
        let ident = parser.take_ident()?.value;

        parser.take_token_if(TokenType::OpenParen)?;
        let kind = match ident.as_str() {
            "true" => Self::True,
            "false" => Self::False,
            "assert_type" => {
                let expression = Box::new(parser.parse()?);
                parser.take_token_if(TokenType::Comma)?;
                Self::AssertType(expression, parser.parse()?)
            }
            "addr" => Self::Addr(Box::new(parser.parse()?)),
            "sizeof" => Self::SizeOf(parser.parse()?),
            "load" => {
                let expr = parser.parse()?;
                let span = expr.span.clone();
                parser.take_token_if(TokenType::Comma)?;
                Self::AssertType(
                    Box::new(Expression::IntrinsicCall(Self::Load(Box::new(expr))).spanned(span)),
                    parser.parse()?,
                )
            }
            "store" => {
                let pointer = Box::new(parser.parse()?);
                parser.take_token_if(TokenType::Comma)?;
                let expression = Box::new(parser.parse()?);
                Self::Store {
                    pointer,
                    expression,
                }
            }
            token if let Ok(operator) = IntrinsicOperator::from_str(token, parser) => {
                let left = Box::new(parser.parse()?);
                parser.take_token_if(TokenType::Comma)?;
                let right = Box::new(parser.parse()?);

                Self::Binary(left, right, operator)
            }
            _ => return Err(parser.unexpected_ident(Self::IDENTS.to_vec())),
        };

        let end = parser.take_token_if(TokenType::CloseParen)?.end();
        Ok(kind.spanned(start..end))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Constructor {
    pub r#type: Spanned<Type>,
    pub fields: Vec<(Spanned<Ident>, Spanned<Expression>)>,
}

impl Parse for Constructor {
    fn parse(parser: &mut Parser) -> Result<Spanned<Self>, Error> {
        let r#type = parser.parse::<Type>()?;
        let mut fields = Vec::new();

        loop {
            let field = parser.parse::<Ident>()?;
            parser.take_token_if(TokenType::Assignment)?;
            fields.push((field, parser.parse()?));
            if parser.take_token_if(TokenType::Comma).is_err() {
                parser.peek_token_if(TokenType::End)?;
                break;
            } else if parser.peek_token_if(TokenType::End).is_ok() {
                break;
            }
        }
        let start = r#type.start();
        let end = parser.take_token_if(TokenType::End)?.end();
        Ok(Self { r#type, fields }.spanned(start..end))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryPrefix {
    operator: Spanned<UnaryOperator>,
    expression: Spanned<Expression>,
}

impl Parse for UnaryPrefix {
    fn parse(parser: &mut Parser) -> Result<Spanned<Self>, Error> {
        let operator = parser.parse()?;
        let expression = parser.parse()?;
        let span = operator.start()..expression.end();
        Ok(Self {
            operator,
            expression,
        }
        .spanned(span))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Return {
    pub expression: Spanned<Expression>,
}

impl Parse for Return {
    fn parse(parser: &mut Parser) -> Result<Spanned<Self>, Error> {
        let start = parser.take_token_if(TokenType::Return)?.start();
        let expression = parser.parse::<Expression>()?;
        let end = expression.end();
        Ok(Self { expression }.spanned(start..end))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldAccess {
    pub expression: Spanned<Expression>,
    pub ident: Spanned<Ident>,
}

impl FieldAccess {
    fn parse(parser: &mut Parser, mut allow: TermKindBitFields) -> Result<Spanned<Self>, Error> {
        allow.remove(TermKind::FieldAccess);
        let expression = Expression::parse_term(parser, allow)?;
        parser.take_token_if(TokenType::Dot)?;
        let ident = parser.parse()?;
        let span = expression.start()..ident.end();
        Ok(Self { expression, ident }.spanned(span))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Generixed {
    pub expression: Spanned<Expression>,
    pub generics: Spanned<crate::GenericArguments>,
}

impl Generixed {
    fn parse(parser: &mut Parser, mut allow: TermKindBitFields) -> Result<Spanned<Self>, Error> {
        allow.remove(TermKind::Generixed);
        let expression = Expression::parse_term(parser, allow)?;
        let generics = parser.parse()?;
        let span = expression.start()..generics.end();

        Ok(Self {
            expression,
            generics,
        }
        .spanned(span))
    }
}

#[derive(Debug, Clone, PartialEq)]
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
    Generixed(Box<Generixed>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, bitfields::BitFields)]
pub enum TermKind {
    Call,
    Generixed,
    FieldAccess,
    // Constructor,
    // Return,
    // IntrinsicCall,
}

impl Expression {
    fn parse_term(parser: &mut Parser, allow: TermKindBitFields) -> Result<Spanned<Self>, Error> {
        if parser.take_token_if(TokenType::OpenParen).is_ok() {
            let expression = parser.parse()?;
            parser.take_token_if(TokenType::CloseParen)?;
            return Ok(expression);
        }

        if allow.contains(TermKind::Call) {
            if let Ok(value) =
                parser.scope(|parser| Call::parse(parser, allow).map_spanned(Self::Call))
            {
                return Ok(value);
            }
        }

        if let Ok(value) = parser.parse().map_spanned(Self::Constructor) {
            return Ok(value);
        }

        if allow.contains(TermKind::Generixed) {
            if let Ok(value) = parser.scope(|parser| {
                Generixed::parse(parser, allow)
                    .map_spanned(Box::new)
                    .map_spanned(Self::Generixed)
            }) {
                return Ok(value);
            }
        }

        if allow.contains(TermKind::FieldAccess) {
            if let Ok(value) = parser.scope(|parser| {
                FieldAccess::parse(parser, allow)
                    .map_spanned(Box::new)
                    .map_spanned(Self::FieldAccess)
            }) {
                return Ok(value);
            }
        }

        if let Ok(value) = parser
            .parse()
            .map_spanned(Box::new)
            .map_spanned(Self::Return)
        {
            return Ok(value);
        }

        if let Ok(value) = parser.parse().map_spanned(Self::IntrinsicCall) {
            return Ok(value);
        }

        parser
            .parse()
            .map_spanned(Self::Literal)
            .or_else(|_| parser.parse().map_spanned(Self::If))
            .or_else(|_| parser.parse().map_spanned(Self::Ident))
            .or_else(|_| {
                parser
                    .parse()
                    .map_spanned(Box::new)
                    .map_spanned(Self::UnaryPrefix)
            })
    }
}

impl Parse for Expression {
    fn parse(parser: &mut Parser) -> Result<Spanned<Self>, Error> {
        binary::Terms::parse(parser).map(Spanned::<Self>::from)
    }
}

pub mod prelude {
    pub use super::control_flow::If;
    pub use super::{binary, Literal};
}
