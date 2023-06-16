use super::Operator;
use crate::{prelude::*, Expression as GenericExpression};

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
        write!(f, "{}{}", self.operator, self.operand)
    }
}

impl NomParse for Expression {
    fn parse(input: &str) -> IResult<Self> {
        map(
            pair(Operator::parse, GenericExpression::parse),
            |(operator, operand)| Self::new(operator, operand),
        )(input)
    }
}
