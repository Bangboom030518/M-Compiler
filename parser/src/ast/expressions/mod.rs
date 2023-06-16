use crate::prelude::*;
pub use accessor::{Call as CallExpression, Namespace as NamespaceAccess};
pub use binary::{Expression as BinaryExpression, Operator as BinaryOperator};
pub use literal::*;
use rand::{distributions::Standard, prelude::*};
pub use unary::{Expression as UnaryExpression, Operator as UnaryOperator};

mod accessor;
mod binary;
mod literal;
mod unary;
use super::Identifier;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    Binary(BinaryExpression),
    Unary(UnaryExpression),
    Call(CallExpression),
    Namespace(NamespaceAccess),
    Identifier(Identifier),
    Literal(Literal),
}

impl Expression {
    fn parse_term(input: &str) -> IResult<Self> {
        whitespace_delimited(map(Literal::parse, Expression::Literal))(input)
    }
}

impl NomParse for Expression {
    fn parse(input: &str) -> IResult<Self> {
        alt((
            // map(BinaryExpression::parse, Expression::Binary),
            // map(UnaryExpression::parse, Expression::Unary),
            // map(CallExpression::parse, Expression::Call),
            // map(NamespaceAccess::parse, Expression::Namespace),
            // map(Identifier::parse, Expression::Identifier),
            map(Literal::parse, Expression::Literal),
        ))(input)
    }
}

impl Distribution<Expression> for Standard {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> Expression {
        Expression::Literal(rng.gen())
    }
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Literal(literal) => write!(f, "{literal}"),
            _ => write!(f, ""), // TODO: finish
        }
    }
}
