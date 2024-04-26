use crate::parser::Parser;
use crate::{Error, Parse};
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
        operator: Spanned<Operator>,
        right: Spanned<super::Expression>,
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
                    .with_spanned(Self::Minus)
            })
            .or_else(|_| {
                parser
                    .take_token_if(TokenType::Divide)
                    .with_spanned(Self::Divide)
            })
            .or_else(|_| {
                parser
                    .take_token_if(TokenType::Remainder)
                    .with_spanned(Self::Remainder)
            })
            .or_else(|_| {
                parser
                    .take_token_if(TokenType::Exponent)
                    .with_spanned(Self::Exponent)
            })
            .or_else(|_| {
                parser
                    .take_token_if(TokenType::Equal)
                    .with_spanned(Self::Exponent)
            })
            .or_else(|_| {
                parser
                    .take_token_if(TokenType::NotEqual)
                    .with_spanned(Self::NotEqual)
            })
            .or_else(|_| {
                parser
                    .take_token_if(TokenType::GreaterThan)
                    .with_spanned(Self::GreaterThan)
            })
            .or_else(|_| {
                parser
                    .take_token_if(TokenType::LessThan)
                    .with_spanned(Self::LessThan)
            })
            .or_else(|_| {
                parser
                    .take_token_if(TokenType::GreaterThanOrEqual)
                    .with_spanned(Self::GreaterThanOrEqual)
            })
            .or_else(|_| {
                parser
                    .take_token_if(TokenType::LessThanOrEqual)
                    .with_spanned(Self::LessThanOrEqual)
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
            | Self::LessThanOrEqual => (1, 0),
            Self::Plus | Self::Minus => (3, 2),
            Self::Multiply | Self::Divide | Self::Remainder => (5, 4),
            Self::Exponent => (6, 7),
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
    pub(crate) fn parse(parser: &mut Parser) -> Result<Self, Error> {
        let left_term = super::Expression::parse_term(parser, super::TermKindBitFields::full())?;
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
            left_term: mut term,
            mut right_terms,
            ..
        } = value;

        while let Some(right_term) = right_terms.pop() {
            let left_binding_power = right_term.value.operator.value.binding_powers().0;
            let right_binding_power = right_terms
                .last()
                .map_or(255, |term| term.value.operator.value.binding_powers().1);

            if left_binding_power < right_binding_power {
                let start = right_term.value.expression.start();
                let end = right_terms
                    .first()
                    .map_or_else(|| term.end(), tokenizer::Spanned::end);

                return super::Expression::Binary(Expression::new(
                    term,
                    right_term.value.operator,
                    Terms {
                        left_term: right_term.value.expression,
                        right_terms,
                    }
                    .into(),
                ))
                .spanned(start..end);
            }

            let span = term.start()..right_term.end();
            term = super::Expression::Binary(Expression::new(
                term,
                right_term.value.operator,
                right_term.value.expression,
            ))
            .spanned(span);
        }

        term
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
        let expression = super::Expression::parse_term(parser, super::TermKindBitFields::full())?;
        let span = operator.start()..expression.end();
        Ok(Self {
            operator,
            expression,
        }
        .spanned(span))
    }
}

#[cfg(test)]
macro_rules! assert_binary_expression {
    ($expr:expr, $left:ident => $left_assert:stmt, $op:expr, $right:ident => $right_assert:stmt) => {
        {
            let super::Expression::Binary(Expression {
                left,
                right,
                operator,
            }) = $expr
            else {
                panic!("Non-binary expression parsed: {:?}", $expr)
            };

            let $left = left.value;
            let $right = right.value;

            $left_assert
            assert_eq!(operator.value, $op);
            $right_assert
        }
    };
}

#[test]
fn binary_expression_parses() {
    let source = r"1 + 2 * 3 / 4";
    let expression = Parser::from(tokenizer::Tokenizer::from(source))
        .parse::<super::Expression>()
        .unwrap();

    assert_binary_expression!(
        expression.value,
        left => assert_eq!(
            left,
            super::Expression::Literal(super::Literal::Integer(1))
        ),
        Operator::Plus,
        right => assert_binary_expression!(
            right,
            left => assert_binary_expression!(
                left,
                left => assert_eq!(
                    left,
                    super::Expression::Literal(super::Literal::Integer(2))
                ),
                Operator::Multiply,
                right => assert_eq!(
                    right,
                    super::Expression::Literal(super::Literal::Integer(3))
                )
            ),
            Operator::Divide,
            right => assert_eq!(
                right,
                super::Expression::Literal(super::Literal::Integer(4))
            )
        )
    );

    let source = r"1 ** 2 ** 3";
    let expression = Parser::from(tokenizer::Tokenizer::from(source))
        .parse::<super::Expression>()
        .unwrap();

    assert_binary_expression!(
        expression.value,
        left => assert_eq!(
            left,
            super::Expression::Literal(super::Literal::Integer(1))
        ),
        Operator::Exponent,
        right => assert_binary_expression!(
            right,
            left => assert_eq!(
                left,
                super::Expression::Literal(super::Literal::Integer(2))
            ),
            Operator::Exponent,
            right => assert_eq!(
                right,
                super::Expression::Literal(super::Literal::Integer(3))
            )
        )
    );
}
