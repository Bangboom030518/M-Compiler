use crate::{Expression as GenericExpression, prelude::*};
use super::Operator;

#[derive(Debug, Clone, PartialEq, Eq, Rand)]
#[exclude_test]
pub struct Expression {
    pub left: Box<GenericExpression>,
    pub right: Box<GenericExpression>,
    pub operator: Operator,
}

impl Expression {
    #[must_use]
    pub fn new(left: GenericExpression, right: GenericExpression, operator: Operator) -> Self {
        Self {
            left: Box::new(left),
            right: Box::new(right),
            operator,
        }
    }
}
