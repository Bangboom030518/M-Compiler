use crate::internal::prelude::*;

#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    pub left: Box<super::Expression>,
    pub right: Box<super::Expression>,
    pub operator: Operator,
}

impl Expression {
    #[must_use]
    pub fn new(left: super::Expression, right: super::Expression, operator: Operator) -> Self {
        Self {
            left: Box::new(left),
            right: Box::new(right),
            operator,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Operator {
    Multiply,
    Plus,
    Minus,
    Divide,
    Remainder,
    Exponent,
    Equal,
    NotEqual,
    GreaterThan,
    LessThan,
    GreaterThanOrEqual,
    LessThanOrEqual,
}

impl Parse for Operator {
    fn parse<'a>(parser: &mut Parser<'a>) -> Option<Self> {
        let operator = match parser.take_token()? {
            Token::Multiply => Self::Multiply,
            Token::Plus => Self::Plus,
            Token::Minus => Self::Minus,
            Token::Divide => Self::Divide,
            Token::Remainder => Self::Remainder,
            Token::Exponent => Self::Exponent,
            Token::Equal => Self::Equal,
            Token::NotEqual => Self::NotEqual,
            Token::GreaterThan => Self::GreaterThan,
            Token::LessThan => Self::LessThan,
            Token::GreaterThanOrEqual => Self::GreaterThanOrEqual,
            Token::LessThanOrEqual => Self::LessThanOrEqual,
            _ => return None,
        };
        Some(operator)
    }
}

impl Operator {
    #[must_use]
    pub const fn binding_powers(self) -> (u8, u8) {
        match self {
            Self::Equal
            | Self::NotEqual
            | Self::GreaterThan
            | Self::LessThan
            | Self::GreaterThanOrEqual
            | Self::LessThanOrEqual => (0, 1),
            Self::Plus | Self::Minus => (2, 3),
            Self::Multiply | Self::Divide | Self::Remainder => (4, 5),
            Self::Exponent => (7, 6),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Terms {
    // The first term in a sequence of binary expressions (e.g. `1` in `1 + 2 * 3`)
    left_term: super::Expression,
    /// The operators and expressions to the right of `left_term`. stored in the reverse order to that which they appear in the expression
    right_terms: Vec<Term>,
}

impl Parse for Terms {
    fn parse<'a>(parser: &mut Parser<'a>) -> Option<Self> {
        let left_term = super::Expression::parse_term(parser)?;
        let mut right_terms = Vec::new();
        while let Some(term) = parser.parse() {
            right_terms.push(term);
        }
        right_terms.reverse();

        Some(Self {
            left_term,
            right_terms,
        })
    }
}

impl From<Terms> for super::Expression {
    /// Reduces terms to one expression
    fn from(value: Terms) -> Self {
        let Terms {
            mut left_term,
            mut right_terms,
        } = value;

        while let Some(Term {
            operator,
            expression,
        }) = right_terms.pop()
        {
            let left_binding_power = operator.binding_powers().0;
            let right_binding_power = right_terms
                .last()
                .map_or(255, |Term { operator, .. }| operator.binding_powers().1);

            if left_binding_power < right_binding_power {
                let right_term = Terms {
                    left_term: expression,
                    right_terms,
                }
                .into();
                return Self::Binary(Expression::new(left_term, right_term, operator));
            }

            left_term = Self::Binary(Expression::new(left_term, expression, operator));
        }
        left_term
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Term {
    pub operator: Operator,
    pub expression: super::Expression,
}

impl Parse for Term {
    fn parse<'a>(parser: &mut Parser<'a>) -> Option<Self> {
        Some(Self {
            operator: parser.parse()?,
            expression: super::Expression::parse_term(parser)?,
        })
    }
}

#[test]
fn binary_expression_parses() {
    let source = r"1 + 1 * 1 / 1";
    assert_eq!(
        Parser::from(Tokenizer::from(source))
            .parse::<super::Expression>()
            .unwrap(),
        super::Expression::Binary(Expression {
            left: Box::new(super::Expression::Literal(super::Literal::Integer(1))),
            operator: Operator::Plus,
            right: Box::new(super::Expression::Binary(Expression {
                left: Box::new(super::Expression::Literal(super::Literal::Integer(1))),
                operator: Operator::Multiply,
                right: Box::new(super::Expression::Binary(Expression {
                    left: Box::new(super::Expression::Literal(super::Literal::Integer(1))),
                    operator: Operator::Divide,
                    right: Box::new(super::Expression::Literal(super::Literal::Integer(1)))
                })),
            }))
        })
    );
}
