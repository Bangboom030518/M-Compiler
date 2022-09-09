use super::expect_single_child;
use crate::{print_tree, Pair, Pairs, Rule};
pub use binary::{BinaryExpression, BinaryOperator};

mod binary;

// TODO: other numeric types?
#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    String(String),
    Number(f64),
    Char(char),
    Bool(bool),
}

impl<'a> From<Pair<'a>> for Literal {
    fn from(pair: Pair<'a>) -> Self {
        let literal = expect_single_child(pair);
        match literal.as_rule() {
            Rule::char_literal => {
                let ch = expect_single_child(literal);
                Self::Char(parse_char(&ch))
            }
            Rule::number_literal => {
                let value = literal.as_span().as_str();
                Self::Number(
                    value
                        .parse::<f64>()
                        .unwrap_or_else(|_| panic!("'{}' is not a number", value)),
                )
            }
            Rule::string_literal => {
                let content = expect_single_child(literal)
                    .into_inner()
                    .map(|ch| parse_char(&ch))
                    .collect::<String>();
                Self::String(content)
            }
            Rule::boolean_literal => match expect_single_child(literal).as_rule() {
                Rule::true_keyword => Self::Bool(true),
                Rule::false_keyword => Self::Bool(false),
                rule => unreachable!("'{:?}' isn't a boolean!", rule),
            },
            rule => unreachable!(
                "What is '{:?}'? It's meant to be a literal you idiot.",
                rule
            ),
        }
    }
}

fn parse_char(ch: &Pair) -> char {
    match ch.as_span().as_str() {
        r"\r" => '\r',
        r"\n" => '\n',
        r"\t" => '\t',
        r"\\" => '\\',
        "\\\"" => '\"',
        ch if ch.starts_with(r"\u") => {
            let digits = ch.replace(r"\u", "");
            let charcode = u32::from_str_radix(&digits, 16)
                .unwrap_or_else(|_| panic!("'{}' are invalid hex digits", &digits));
            char::from_u32(charcode)
                .unwrap_or_else(|| panic!("'{}' is not a valid charcode", charcode))
        }
        ch => ch
            .chars()
            .next()
            .expect("Expected string with 1 char, found empty"),
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum UnaryOperator {
    Bang,
    Negate,
}

impl<'a> From<Pair<'a>> for UnaryOperator {
    fn from(pair: Pair<'a>) -> Self {
        let operator = expect_single_child(pair);

        match operator.as_rule() {
            Rule::negate => Self::Negate,
            Rule::bang => Self::Bang,
            rule => unreachable!("'{:?}' is not a valid unary operator", rule),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct UnaryExpression {
    pub operator: UnaryOperator,
    pub operand: Box<Expression>,
}

impl<'a> From<Pair<'a>> for UnaryExpression {
    fn from(pair: Pair<'a>) -> Self {
        let mut inner = pair.into_inner();
        let operator = inner.next().expect("Unary expression needs an operator");
        let operand = inner.next().expect("Unary expression needs an operand");
        assert_eq!(
            operator.as_rule(),
            Rule::unary_operator,
            "First child of unary expression should be 'unary_operator'. Found '{:?}'",
            operator.as_rule()
        );
        assert_eq!(
            operand.as_rule(),
            Rule::expression,
            "Second child of unary expression should be 'unary_expression'. Found '{:?}'",
            operand.as_rule()
        );
        Self {
            operator: UnaryOperator::from(operator),
            operand: Box::new(Expression::from(operand)),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Literal(Literal),
    Binary(BinaryExpression),
    Unary(UnaryExpression),
    Identifier(String),
}

impl<'a> From<Pair<'a>> for Expression {
    fn from(pair: Pair<'a>) -> Self {
        let pair = expect_single_child(pair);
        match pair.as_rule() {
            Rule::binary_expression => Self::Binary(BinaryExpression::from(pair)),
            Rule::unary_expression => Self::Unary(UnaryExpression::from(pair)),
            Rule::literal => Self::Literal(Literal::from(pair)),
            Rule::group => Self::from(expect_single_child(pair)),
            Rule::identifier => {
                Self::Identifier(expect_single_child(pair).as_span().as_str().to_string())
            }
            // TODO: write function to automate this
            rule => unreachable!("'{:?}' is not a valid expression.", rule),
        }
    }
}
