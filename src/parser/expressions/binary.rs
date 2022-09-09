use super::{expect_single_child, Expression};
use crate::{print_tree, Pair, Rule};
use std::collections::{HashMap, HashSet};

#[derive(Debug, PartialEq, Clone)]
pub struct BinaryExpression {
    pub operator: BinaryOperator,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

impl<'a> From<Pair<'a>> for BinaryExpression {
    fn from(pair: Pair<'a>) -> Self {
        let mut terms = Vec::<(Expression, HashSet<usize>)>::new();
        let mut operators = Vec::<(usize, BinaryOperator)>::new();
        let pairs = pair.into_inner();

        let mut i = 0;

        for pair in pairs {
            if pair.as_rule() == Rule::binary_term {
                terms.push((Expression::from(pair), HashSet::from([i])));
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

        for operator in operators {
            let (index, operator) = operator;

            // Operator index is the index of the lhs
            let left_index = index;
            let (left, left_indices) = terms
                .get(left_index)
                .unwrap_or_else(|| panic!("Couldn't find lhs of binary expression at index {}. Operator is at index {}", left_index, index));

            // Operator index + 1 is the index of the rhs
            let right_index = index + 1;
            let (right, right_indices) = terms
                .get(right_index)
                .unwrap_or_else(|| panic!("Couldn't find rhs of binary expression at index {}. Operator is at index {}", right_index, index));

            let expression = BinaryExpression {
                left: Box::new(left.clone()),
                right: Box::new(right.clone()),
                operator,
            };

            // join left and right indices
            let indices: HashSet<usize> = left_indices.union(right_indices).copied().collect();

            // replace all used indices with created expression
            for index in &indices {
                terms[*index] = (Expression::Binary(expression.clone()), indices.clone());
            };
        };

        // Get any expression in the list, it has spread to all indices
        if let Expression::Binary(expression) = &terms[0].0 {
            expression.clone()
        } else {
            unreachable!("Found non-binary expression in list of binary expressions")
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
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
            rule => unreachable!("'{:?}' isn't a binary operator!", rule),
        }
    }
}

impl BinaryOperator {
    fn precedance(&self) -> u8 {
        match self {
            BinaryOperator::Plus | BinaryOperator::Minus => 0,
            BinaryOperator::Divide | BinaryOperator::Multiply | BinaryOperator::Modulo => 1,
            BinaryOperator::Exponent => 2,
            BinaryOperator::LogicalAND | BinaryOperator::LogicalOR => 3,
        }
    }
}
