use super::expect_single_child;
use crate::{Pair, Rule};

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
