use crate::{Pair, Rule};
use super::{Expression as GenericExpression, expect_single_child};

#[derive(Debug, PartialEq, Clone)]
pub struct Expression {
    pub operator: Operator,
    pub operand: Box<GenericExpression>,
}

impl<'a> From<Pair<'a>> for Expression {
    fn from(pair: Pair<'a>) -> Self {
        let mut inner = pair.into_inner();
        let operator = inner.next().expect("Unary expression needs an operator");
        let operand = inner.next().expect("Unary expression needs an operand");
        assert_eq!(
            operator.as_rule(),
            Rule::unary_operator,
            "First child of unary expression should be 'unary_operator'. Found '{:?}'",
            operator.as_rule()
        );
        assert_eq!(
            operand.as_rule(),
            Rule::binary_term,
            "Second child of unary expression should be 'binary_term'. Found '{:?}'",
            operand.as_rule()
        );
        Self {
            operator: Operator::from(operator),
            operand: Box::new(GenericExpression::from_binary_term(operand)),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Operator {
    Bang,
    Negate,
}

impl<'a> From<Pair<'a>> for Operator {
    fn from(pair: Pair<'a>) -> Self {
        let operator = expect_single_child(pair);

        match operator.as_rule() {
            Rule::negate => Self::Negate,
            Rule::bang => Self::Bang,
            rule => unreachable!("'{:?}' is not a valid unary operator", rule),
        }
    }
}
