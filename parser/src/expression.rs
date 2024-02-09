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
    fn from_str(string: &str) -> Option<Self> {
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

#[derive(PartialEq, Debug, Clone)]
pub enum IntrinsicCall {
    AssertType(Box<Expression>, Type),
    MutablePointer(Box<Expression>),
    Deref(Box<Expression>),
    Binary(Box<Expression>, Box<Expression>, IntrinsicOperator),
}

impl Parse for IntrinsicCall {
    fn parse(parser: &mut Parser) -> Option<Self> {
        parser.take_token_if(&Token::At)?;
        let Token::Ident(ident) = parser.take_token()? else {
            return None;
        };

        parser.take_token_if(&Token::OpenParen)?;
        let value = match ident.as_str() {
            "assert_type" => {
                let expression = Box::new(parser.parse()?);
                parser.take_token_if(&Token::Comma)?;
                Self::AssertType(expression, parser.parse()?)
            },
            "mutable_pointer" => {
                Self::MutablePointer(Box::new(parser.parse()?))
            },
            "deref" => {
                Self::Deref(Box::new(parser.parse()?))
            },
            
            token if let Some(op) = IntrinsicOperator::from_str(token) => {
                let left = Box::new(parser.parse()?);
                parser.take_token_if(&Token::Comma)?;
                let right = Box::new(parser.parse()?);

                Self::Binary(left, right, op)
            },
            _ => return None,
        };

        parser.take_token_if(&Token::CloseParen)?;
        Some(value)
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Constructor {
    pub r#type: Type,
    pub fields: Vec<(Ident, Box<Expression>)>,
}

impl Parse for Constructor {
    fn parse(parser: &mut Parser) -> Option<Self> {
        let r#type = parser.parse()?;
        let mut fields = Vec::new();

        loop {
            let field = parser.parse::<Ident>()?;
            parser.take_token_if(&Token::Assignment)?;
            fields.push((field, Box::new(parser.parse()?)));
            if parser.take_token_if(&Token::Comma).is_none() {
                parser.take_token_if(&Token::End)?;
                break;
            } else if parser.take_token_if(&Token::End).is_some() {
                break;
            }
        }

        Some(Self { r#type, fields })
    }
}

#[derive(PartialEq, Debug, Clone)]
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
    fn parse_term(parser: &mut Parser) -> Option<Self> {
        if parser.take_token_if(&Token::OpenParen).is_some() {
            let expression = parser.parse()?;
            parser.take_token_if(&Token::CloseParen)?;
            return Some(expression);
        }
        // TODO: parser.scope(|parser| {})
        parser
            .parse::<Call>()
            .map(Self::Call)
            .or_else(|| {
                parser.scope(|parser| {
                    // TODO: may not be ident
                    let expr = Box::new(parser.parse().map(Self::Ident)?);
                    parser.take_token_if(&Token::Dot)?;
                    Some(Self::FieldAccess(expr, parser.parse()?))
                })
            })
            .or_else(|| parser.parse().map(Self::Constructor))
            .or_else(|| {
                parser.scope(|parser| {
                    parser.take_token_if(&Token::Return)?;
                    Some(Self::Return(Box::new(parser.parse()?)))
                })
            })
            .or_else(|| parser.parse().map(Self::IntrinsicCall))
            .or_else(|| Self::parse_nonpostfix_term(parser))
    }

    fn parse_nonpostfix_term(parser: &mut Parser) -> Option<Self> {
        if parser.take_token_if(&Token::OpenParen).is_some() {
            let expression = parser.parse()?;
            parser.take_token_if(&Token::CloseParen)?;
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
    fn parse(parser: &mut Parser) -> Option<Self> {
        Some(parser.parse::<binary::Terms>()?.into())
    }
}

pub mod prelude {
    pub use super::control_flow::If;
    pub use super::{binary, Literal};
}
