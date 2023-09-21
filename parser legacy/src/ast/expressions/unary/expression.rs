use super::{super::binary::fmt_expression, Operator};
use crate::{ast::prelude::*, prelude::*, Expression as GenericExpression};
use rand_derive::Rand;
#[derive(Debug, Clone, PartialEq, Eq, Rand)]
pub struct Expression {
    pub operator: Operator,
    pub operand: Box<GenericExpression>,
}

impl Expression {
    #[must_use]
    pub fn new(operator: Operator, operand: GenericExpression) -> Self {
        Self {
            operator,
            operand: Box::new(operand),
        }
    }
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let Self { operator, operand } = self;
        write!(f, "{operator}")?;
        if let GenericExpression::Literal(Literal::Number(number)) = *operand.clone() {
            if number.sign() == Sign::Positive {
                return write!(f, "({operand})");
            }
        }
        fmt_expression(f, operand)
    }
}

impl Parse for Expression {
    fn parse(input: &str) -> IResult<Self> {
        map(
            pair(Operator::parse, GenericExpression::parse_term),
            |(operator, operand)| Self::new(operator, operand),
        )(input)
    }
}

// impl Parse for Expression {
//     fn parse(input: &str) -> IResult<Self> {
// map(
//     pair(Operator::parse, GenericExpression::parse_term),
//     |(operator, operand)| Self::new(operator, operand),
// )(input)
//     }
// }
