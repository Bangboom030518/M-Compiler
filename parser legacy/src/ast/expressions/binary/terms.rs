use super::{term::Term, Expression, fmt_expression};
use crate::{prelude::*, Expression as GenericExpression};

#[derive(Debug, Clone, PartialEq, Eq, Rand)]
pub struct Terms {
    // The first term in a sequence of binary expressions (e.g. `1` in `1 + 2 * 3`)
    left_term: GenericExpression,
    /// The operators and expressions to the right of `left_term`. stored in the reverse order to that which they appear in the expression
    right_terms: Vec<Term>,
}

impl Parse for Terms {
    fn parse(input: &str) -> IResult<Self> {
        let (input, left_term) = GenericExpression::parse_term(input)?;
        let (input, right_terms) = map(many0(Term::parse), |terms| {
            terms.into_iter().rev().collect()
        })(input)?;

        Ok((
            input,
            Self {
                left_term,
                right_terms,
            },
        ))
    }
}

impl From<Terms> for GenericExpression {
    /// Reduces terms to one expression
    fn from(value: Terms) -> Self {
        let Terms {
            mut left_term,
            right_terms: mut right,
        } = value;

        while let Some(Term {
            operator,
            expression,
        }) = right.pop()
        {
            let left_binding_power = operator.binding_powers().1;
            let right_binding_power = right
                .last()
                .map_or(255, |Term { operator, .. }| operator.binding_powers().0);

            if left_binding_power < right_binding_power {
                let right_term = Terms {
                    left_term: expression,
                    right_terms: right,
                }
                .into();
                return Self::Binary(Expression::new(left_term, right_term, operator));
            }

            left_term = Self::Binary(Expression::new(left_term, expression, operator));
        }
        left_term
    }
}

impl std::fmt::Display for Terms {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let Self {
            left_term,
            right_terms,
        } = self;
        fmt_expression(f, left_term)?;
        write!(
            f,
            "{}",
            right_terms
                .iter()
                .rev()
                .map(ToString::to_string)
                .collect::<String>()
        )
    }
}
