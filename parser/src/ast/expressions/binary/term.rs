use super::{Operator, fmt_expression};
use crate::{prelude::*, Expression as GenericExpression};

#[derive(Debug, Clone, PartialEq, Eq, Rand)]
pub struct Term {
    pub operator: Operator,
    pub expression: GenericExpression,
}

impl Term {
    pub const fn new(operator: Operator, expression: GenericExpression) -> Self {
        Self {
            operator,
            expression,
        }
    }
}

impl Parse for Term {
    fn parse(input: &str) -> IResult<Self> {
        map(
            pair(
                whitespace_delimited(Operator::parse),
                GenericExpression::parse_term,
            ),
            |(operator, expression)| Self::new(operator, expression),
        )(input)
    }
}

impl std::fmt::Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let Self { operator, expression } = self;
        write!(f, "{operator} ")?;
        fmt_expression(f, expression)
    }
}
