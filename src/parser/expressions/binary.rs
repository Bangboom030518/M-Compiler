use crate::{Rule, Pair, print_tree};
use super::{Expression, expect_single_child};
use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub struct BinaryExpression {
    pub operator: BinaryOperator,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

impl<'a> From<Pair<'a>> for BinaryExpression {
    fn from(pair: Pair<'a>) -> Self {
        let mut terms = Vec::<Expression>::new();
        let mut operators = Vec::<(usize, BinaryOperator)>::new();
        let pairs = pair.into_inner();

        let mut i = 0;

        for pair in pairs {
            if pair.as_rule() == Rule::binary_term {
                terms.push(Expression::from(pair));
            } else {
                operators.push((i, BinaryOperator::from(pair)));
                i += 1;
            };
        }
        operators.sort_by(|first, second| {
            let first = first.1.precedance();
            let second = second.1.precedance();
            second.cmp(&first)
        });

        // let result = terms.into_iter();

        // (Expression, [1, 2])

        for operator in operators {
            let (index, operator) = operator;
            let left_index = index * 2;
            let left = terms.get(left_index).expect("Couldn't find lhs of binary expression");
            let right_index = left_index + 2;
            let right = terms.get(right_index).expect("Couldn't find rhs of binary expression");

            terms

        }

        unimplemented!("'Binary Expression'")
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Multiply,
    Divide,
    Exponent,
    Modulo,
    LogicalOR,
    LogicalAND,
}

impl<'a> From<Pair<'a>> for BinaryOperator {
    fn from(pair: Pair<'a>) -> Self {
        match expect_single_child(pair).as_rule() {
            Rule::plus => BinaryOperator::Plus,
            Rule::multiply => BinaryOperator::Multiply,
            Rule::minus => BinaryOperator::Minus,
            Rule::modulo => BinaryOperator::Modulo,
            Rule::divide => BinaryOperator::Divide,
            rule => unreachable!("'{:?}' isn't a binary operator!", rule)
        }
    }
}

impl BinaryOperator {
    fn precedance(&self) -> u8 {
        match self {
            BinaryOperator::Plus | BinaryOperator::Minus => 0,
            BinaryOperator::Divide | BinaryOperator::Multiply | BinaryOperator::Modulo => 1,
            BinaryOperator::Exponent => 2,
            BinaryOperator::LogicalAND | BinaryOperator::LogicalOR => 3
        }
    }
}