use crate::{parser::Parser, Error, Parse};
use tokenizer::{AsSpanned, Spanned, SpannedResultExt, TokenType};

#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    pub left: Box<Spanned<super::Expression>>,
    pub right: Box<Spanned<super::Expression>>,
    pub operator: Spanned<Operator>,
}

impl Expression {
    #[must_use]
    pub fn new(
        left: Spanned<super::Expression>,
        right: Spanned<super::Expression>,
        operator: Spanned<Operator>,
    ) -> Self {
        Self {
            left: Box::new(left),
            right: Box::new(right),
            operator,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
    fn parse(parser: &mut Parser) -> Result<Spanned<Self>, Error> {
        parser
            .take_token_if(TokenType::Multiply)
            .with_spanned(Self::Multiply)
            .or_else(|_| {
                parser
                    .take_token_if(TokenType::Plus)
                    .with_spanned(Self::Plus)
            })
            .or_else(|_| {
                parser
                    .take_token_if(TokenType::Minus)
                    .with_spanned(Operator::Minus)
            })
            .or_else(|_| {
                parser
                    .take_token_if(TokenType::Divide)
                    .with_spanned(Operator::Divide)
            })
            .or_else(|_| {
                parser
                    .take_token_if(TokenType::Remainder)
                    .with_spanned(Operator::Remainder)
            })
            .or_else(|_| {
                parser
                    .take_token_if(TokenType::Exponent)
                    .with_spanned(Operator::Exponent)
            })
            .or_else(|_| {
                parser
                    .take_token_if(TokenType::Equal)
                    .with_spanned(Operator::Exponent)
            })
            .or_else(|_| {
                parser
                    .take_token_if(TokenType::NotEqual)
                    .with_spanned(Operator::NotEqual)
            })
            .or_else(|_| {
                parser
                    .take_token_if(TokenType::GreaterThan)
                    .with_spanned(Operator::GreaterThan)
            })
            .or_else(|_| {
                parser
                    .take_token_if(TokenType::LessThan)
                    .with_spanned(Operator::LessThan)
            })
            .or_else(|_| {
                parser
                    .take_token_if(TokenType::GreaterThanOrEqual)
                    .with_spanned(Operator::GreaterThanOrEqual)
            })
            .or_else(|_| {
                parser
                    .take_token_if(TokenType::LessThanOrEqual)
                    .with_spanned(Operator::LessThanOrEqual)
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

#[derive(Debug, Clone)]
pub struct Terms {
    /// The first term in a sequence of binary expressions (e.g. `1` in `1 + 2 * 3`)
    left_term: Spanned<super::Expression>,
    /// The operators and expressions to the right of `left_term`. stored in the reverse order to that which they appear in the expression
    right_terms: Vec<Spanned<Term>>,
}

impl Terms {
    pub fn parse(parser: &mut Parser) -> Result<Self, Error> {
        let left_term = super::Expression::parse_term(parser)?;
        let mut right_terms = Vec::new();
        while let Ok(term) = parser.parse() {
            right_terms.push(term);
        }

        right_terms.reverse();

        Ok(Self {
            left_term,
            right_terms,
        })
    }
}

impl From<Terms> for Spanned<super::Expression> {
    /// Reduces terms to one expression
    fn from(value: Terms) -> Self {
        let Terms {
            mut left_term,
            mut right_terms,
            ..
        } = value;

        while let Some(term) = right_terms.pop() {
            let left_binding_power = term.value.operator.value.binding_powers().0;
            let right_binding_power = right_terms
                .last()
                .map_or(255, |term| term.value.operator.value.binding_powers().1);

            if left_binding_power < right_binding_power {
                let start = term.value.expression.start();
                let end = if let Some(term) = right_terms.last() {
                    term.end()
                } else {
                    left_term.end()
                };

                let right_term = Terms {
                    left_term: term.value.expression,
                    right_terms,
                }
                .into();
                return super::Expression::Binary(Expression::new(
                    left_term,
                    right_term,
                    term.value.operator,
                ))
                .spanned(start..end);
            }
            let span = left_term.start()..term.end();
            left_term = super::Expression::Binary(Expression::new(
                left_term,
                term.value.expression,
                term.value.operator,
            ))
            .spanned(span);
        }
        left_term
    }
}

#[derive(Debug, Clone)]
pub struct Term {
    pub operator: Spanned<Operator>,
    pub expression: Spanned<super::Expression>,
}

impl Parse for Term {
    fn parse(parser: &mut Parser) -> Result<Spanned<Self>, Error> {
        let operator = parser.parse()?;
        let expression = super::Expression::parse_term(parser)?;
        let span = operator.start()..expression.end();
        Ok(Self {
            operator,
            expression,
        }
        .spanned(span))
    }
}

#[cfg(ignore)]
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
