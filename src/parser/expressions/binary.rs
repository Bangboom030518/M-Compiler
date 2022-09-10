use super::{expect_single_child, Expression as GenericExpression};
use crate::{Pair, Rule};

#[derive(Debug, PartialEq, Clone)]
pub struct Expression {
    pub operator: Operator,
    pub left: Box<GenericExpression>,
    pub right: Box<GenericExpression>,
}

type Terms = Vec<Box<Term>>;
#[derive(Clone)]
struct Term {
    expression: Box<GenericExpression>,
    start: usize,
    end: usize,
}

impl<'a> From<Pair<'a>> for Expression {
    fn from(pair: Pair<'a>) -> Self {
        let mut terms: Terms = Vec::new();
        let mut operators = Vec::<(usize, Operator)>::new();
        let pairs = pair.into_inner();

        let mut i = 0;

        for pair in pairs {
            if pair.as_rule() == Rule::binary_term {
                let term = Term {
                    expression: Box::new(GenericExpression::from(pair)),
                    start: i,
                    end: i,
                };
                terms.push(Box::new(term));
            } else {
                operators.push((i, Operator::from(pair)));
                i += 1;
            };
        }

        sort_operators(&mut operators);

        for operator in operators {
            let (index, operator) = operator;

            // Operator index is the index of the lhs
            let left_index = index;
            let Term {
                expression: ref left,
                start,
                ..
            } = *terms[left_index];

            // Operator index + 1 is the index of the rhs
            let right_index = index + 1;
            let Term {
                expression: ref right,
                end,
                ..
            } = *terms[right_index];

            let expression = Self {
                left: Box::new(*left.clone()),
                right: Box::new(*right.clone()),
                operator,
            };

            let term = Term {
                expression: Box::new(GenericExpression::Binary(expression)),
                start, end
            };

            terms[start] = Box::new(term.clone());
            terms[end] = Box::new(term);

            // replace all used indices with created expression
        }

        // Get any expression in the list, it has spread to all indices
        if let GenericExpression::Binary(ref expression) = *terms[0].expression {
            expression.clone()
        } else {
            unreachable!("Found non-binary expression in list of binary expressions")
        }
    }
}

/// Sorts the operators by precedance
///
/// # Examples
///
/// ```
/// let mut operators = vec![(0, Operator::Addition), (1, Operator::Division)];
/// sort_operators(&mut operators);
/// assert_eq!(operators, vec![(1, Operator::Division), (0, Operator::Addition)]);
/// ```
fn sort_operators(operators: &mut [(usize, Operator)]) {
    operators.sort_by(|first, second| {
        let first = first.1.precedance();
        let second = second.1.precedance();
        second.cmp(&first)
    });
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Operator {
    // Arithmetic
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Exponentation,
    Modulo,
    // Logical
    LogicalOR,
    LogicalAND,
    // Bitwise
    BitwiseAND,
    BitwiseOR,
    BitwiseXOR,
}

impl<'a> From<Pair<'a>> for Operator {
    fn from(pair: Pair<'a>) -> Self {
        match expect_single_child(pair).as_rule() {
            Rule::addition => Self::Addition,
            Rule::multiplication => Self::Multiplication,
            Rule::subtraction => Self::Subtraction,
            Rule::modulo => Self::Modulo,
            Rule::division => Self::Division,
            Rule::exponentation => Self::Exponentation,
            Rule::logical_or => Self::LogicalOR,
            Rule::logical_and => Self::LogicalAND,
            Rule::bitwise_and => Self::BitwiseAND,
            Rule::bitwise_or => Self::BitwiseOR,
            Rule::bitwise_xor => Self::BitwiseXOR,
            rule => unreachable!("'{:?}' isn't a binary operator!", rule),
        }
    }
}

impl Operator {
    const fn precedance(&self) -> u8 {
        match self {
            Self::Addition | Self::Subtraction => 0,
            Self::Division | Self::Multiplication | Self::Modulo => 1,
            Self::Exponentation => 2,
            Self::BitwiseAND | Self::BitwiseOR | Self::BitwiseXOR => 3,
            Self::LogicalAND | Self::LogicalOR => 4,
        }
    }
}
