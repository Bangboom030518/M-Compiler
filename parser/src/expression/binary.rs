use macros::Spanned;

use crate::internal::prelude::*;
use crate::{Error};

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperatorKind {
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

#[derive(Spanned)]
pub struct Operator {
    kind: OperatorKind,
    #[span]
    span: tokenizer::Span,
}

impl Parse for Operator {
    fn parse(parser: &mut Parser) -> Result<Self, Error> {
        let token = parser.take_token()?;
        let kind = match token.token {
            Token::Multiply => OperatorKind::Multiply,
            Token::Plus => OperatorKind::Plus,
            Token::Minus => OperatorKind::Minus,
            Token::Divide => OperatorKind::Divide,
            Token::Remainder => OperatorKind::Remainder,
            Token::Exponent => OperatorKind::Exponent,
            Token::Equal => OperatorKind::Equal,
            Token::NotEqual => OperatorKind::NotEqual,
            Token::GreaterThan => OperatorKind::GreaterThan,
            Token::LessThan => OperatorKind::LessThan,
            Token::GreaterThanOrEqual => OperatorKind::GreaterThanOrEqual,
            Token::LessThanOrEqual => OperatorKind::LessThanOrEqual,
            _ => return None,
        };
        Ok(Self {
            kind,
            span: token.span,
        })
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

#[derive(Debug, Clone, Spanned)]
pub struct Terms {
    /// The first term in a sequence of binary expressions (e.g. `1` in `1 + 2 * 3`)
    left_term: super::Expression,
    /// The operators and expressions to the right of `left_term`. stored in the reverse order to that which they appear in the expression
    right_terms: Vec<Term>,
    #[span]
    span: tokenizer::Span,
}

impl Parse for Terms {
    fn parse(parser: &mut Parser) -> Result<Self, Error> {
        let left_term = super::Expression::parse_term(parser)?;
        let start = left_term.start();
        let mut right_terms = Vec::new();
        while let Some(term) = parser.parse() {
            right_terms.push(term);
        }

        let end = if let Some(term) = right_terms.last() {
            term.end()
        } else {
            left_term.end()
        };

        right_terms.reverse();

        Some(Self {
            left_term,
            right_terms,
            span: start..end,
        })
    }
}

impl From<Terms> for super::Expression {
    /// Reduces terms to one expression
    fn from(value: Terms) -> Self {
        let Terms {
            mut left_term,
            mut right_terms,
            ..
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
                let start = expression.start();
                let end = if let Some(term) = right_terms.last() {
                    term.end()
                } else {
                    left_term.end()
                };
                let right_term = Terms {
                    left_term: expression,
                    right_terms,
                    span: start..end,
                }
                .into();
                return Self::Binary(Expression::new(left_term, right_term, operator));
            }

            left_term = Self::Binary(Expression::new(left_term, expression, operator));
        }
        left_term
    }
}

#[derive(Debug, Clone)]
pub struct Term {
    pub operator: Operator,
    pub expression: super::Expression,
}

impl Parse for Term {
    fn parse(parser: &mut Parser) -> Result<Self, Error> {
        Some(Self {
            operator: parser.parse()?,
            expression: super::Expression::parse_term(parser)?,
        })
    }
}

impl crate::Spanned for Term {
    fn span(&self) -> tokenizer::Span {
        self.operator.start()..self.expression.end()
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

    let source = r"1 ** 2 ** 3";
    assert_eq!(
        Parser::from(Tokenizer::from(source))
            .parse::<super::Expression>()
            .unwrap(),
        super::Expression::Binary(Expression {
            left: Box::new(super::Expression::Literal(super::Literal::Integer(1))),
            operator: Operator::Exponent,
            right: Box::new(super::Expression::Binary(Expression {
                left: Box::new(super::Expression::Literal(super::Literal::Integer(2))),
                operator: Operator::Exponent,
                right: Box::new(super::Expression::Literal(super::Literal::Integer(3))),
            }))
        })
    );
}
