use super::Operator;
use crate::{prelude::*, Expression as GenericExpression};

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

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let Self {
            left,
            right,
            operator,
        } = self;
        /*
        Expression {
            left: Expression { left: Continue, right: Continue, operator: Add },
            right: Expression { left: Continue, right: Continue, operator: Modulo },
            operator: Add
        }
        */

        if let GenericExpression::Binary(expression) = *left.clone() {
            write!(f, "({expression})")?;
        } else {
            super::fmt_expression(f, left)?;
        }
        // Expression { left: Continue, right: Binary(Expression { left: Continue, right: Continue, operator: Divide }), operator: Divide }
        // (2, 3)
        // / (continue / continue)
        write!(f, " {operator} ")?;

        // 1 + 2 + 3 % 4
        // 1 <0>+<1> 2 <0>+<1> 3 <2>%<3> 4
        // 1, (+ 2), (+ 3), (% 4)

        if let GenericExpression::Binary(expression) = *right.clone() {
            //                     2 < 1
            if expression.operator.binding_powers().0 < operator.binding_powers().1 {
                write!(f, "({expression})")?;
            } else {
                write!(f, "{expression}")?;
            }
        } else {
            super::fmt_expression(f, right)?;
        }
        Ok(())
        // 1 * 2 + 3 * 4
    }
}
