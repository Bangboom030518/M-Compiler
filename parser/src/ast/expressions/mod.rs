use std::fmt::Display;

use self::binary::Terms;

use super::Identifier;
use crate::prelude::*;
pub use accessor::{Call as CallExpression, Namespace as NamespaceAccess};
pub use binary::{Expression as BinaryExpression, Operator as BinaryOperator};
pub use control_flow::{If, While};
pub use literal::*;
// use rand::{distributions::Standard, prelude::*};
pub use unary::{Expression as UnaryExpression, Operator as UnaryOperator};

mod accessor;
mod binary;
mod control_flow;
mod literal;
mod unary;

#[derive(Debug, Clone, PartialEq, Eq, Rand)]
pub struct Assignment {
    pub left: Identifier,
    pub right: Box<Expression>,
}

impl Assignment {
    pub fn new(left: Identifier, right: Expression) -> Self {
        Self {
            left,
            right: Box::new(right),
        }
    }
}

impl Parse for Assignment {
    fn parse(input: &str) -> IResult<Self> {
        // TODO: allow assigments to more complex exxpressions
        map(
            pair(
                terminated(Identifier::parse, whitespace_delimited(char('='))),
                Expression::parse,
            ),
            |(left, right)| Self::new(left, right),
        )(input)
    }
}

impl Display for Assignment {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let Self { left, right } = &self;
        write!(f, "{left} = {right}")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Rand)]
pub enum Expression {
    Assignment(Assignment),
    Binary(BinaryExpression),
    Literal(Literal),
    Unary(UnaryExpression),
    #[skip_variant]
    Call(CallExpression),
    #[skip_variant]
    Namespace(NamespaceAccess),
    If(If),
    While(While),
    #[default_variant]
    Continue,
    Break,
    Return(Box<Expression>),
    Identifier(Identifier),
}

impl Expression {
    fn parse_term(input: &str) -> IResult<Self> {
        alt((
            delimited(char('('), whitespace_delimited(Self::parse), char(')')),
            map(Assignment::parse, Self::Assignment),
            map(Literal::parse, Self::Literal),
            map(UnaryExpression::parse, Self::Unary),
            map(If::parse, Self::If),
            map(While::parse, Self::While),
            value(Self::Continue, tag("continue")),
            value(Self::Break, tag("break")),
            map(
                preceded(pair(tag("return"), whitespace), Self::parse),
                |expression| Self::Return(Box::new(expression)),
            ),
            map(Identifier::parse, Self::Identifier),
        ))(input)
    }
}

impl Parse for Expression {
    fn parse(input: &str) -> IResult<Self> {
        map(Terms::parse, Self::from)(input)
    }
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Assignment(assignment) => write!(f, "{assignment}"),
            Self::Literal(literal) => write!(f, "{literal}"),
            Self::Binary(binary) => write!(f, "{binary}"),
            Self::Unary(unary) => write!(f, "{unary}"),
            Self::While(expression) => write!(f, "{expression}"),
            Self::Break => write!(f, "break"),
            Self::Continue => write!(f, "continue"),
            Self::Return(expression) => write!(f, "return {expression}"),
            Self::Identifier(identifier) => write!(f, "{identifier}"),
            Self::If(expression) => write!(f, "{expression}"),
            Self::Call(_) => todo!(),
            Self::Namespace(_) => todo!(),
        }
    }
}
