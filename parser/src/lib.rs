// TODO: find a way to make parser modular.
use peg::parser;
use std::fmt;

const BINARY_DIGITS: &[char] = &['0', '1'];
const DENARY_DIGITS: &[char] = &['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
const HEX_DIGITS: &[char] = &['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'];

#[derive(Debug)]
pub enum Literal {
    Number(Number),
    List(Vec<Expression>),
    Char(char),
}

#[derive(Debug)]
pub enum Base {
    Binary,
    Octal,
    Denary,
    Hexidecimal,
}

impl std::fmt::Display for Base {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "{}", format!("{:?}", self).to_lowercase())
    }
}

impl TryFrom<u8> for Base {
    type Error = u8;

    fn try_from(number: u8) -> Result<Self, u8> {
        match number {
            2 => Ok(Self::Binary),
            8 => Ok(Self::Octal),
            10 => Ok(Self::Denary),
            16 => Ok(Self::Hexidecimal),
            number => Err(number),
        }
    }
}

#[derive(Debug)]
pub struct Number {
    whole_digits: Vec<u8>,
    fractional_digits: Vec<u8>,
    base: Base,
    positive: bool,
}

#[derive(Debug)]
pub enum BinaryOperator {
    Multiply,
    Add,
    Subtract,
    Divide,
    Exponent,
}

#[derive(Debug)]
pub struct BinaryExpression {
    left: Box<Expression>,
    right: Box<Expression>,
    operator: BinaryOperator,
}

#[derive(Debug)]
pub enum UnaryOperator {
    Negate,
}

#[derive(Debug)]
pub struct UnaryExpression {
    operand: Box<Expression>,
    operator: UnaryOperator,
}

#[derive(Debug)]
pub enum Expression {
    Literal(Literal),
    Binary(BinaryExpression),
    Unary(UnaryExpression),
}

#[allow(clippy::cast_possible_truncation)]
fn digits_from_slice(digits: &[char], base_digits: &[char]) -> Vec<u8> {
    digits
        .iter()
        .map(|&digit| {
            base_digits
                .iter()
                .position(|&character| character == digit)
                .unwrap_or_else(|| {
                    panic!(
                        "Digit {} doesn't exist in base '{}'",
                        digit,
                        match Base::try_from(base_digits.len() as u8) {
                            Ok(base) => format!("{}", base),
                            Err(number) => number.to_string(),
                        }
                    )
                }) as u8
        })
        .collect()
}

parser! {
    grammar m_parser() for str {
        rule whitespace() =  [' ' | '\n' | '\t']

        // Comments like this one
        rule line_comment() = "//" (!"\n" [_])* ("\n" / ![_])

        /* Comments like this */
        rule inline_comment() = "/*" (!"*/" [_])* "*/"

        rule _() = quiet!{ (whitespace() / "\n" / inline_comment() / line_comment())* }

        rule char() -> char
          = character:['a'] {
            character
        }
        
        rule hex_digits() -> Vec<char>
          = digits:(['0'..='9'] / ['a'..='f'])* { digits.to_vec() }

        rule hex_number() -> Number
          = negation_sign:"-"? "0x" whole_digits:hex_digits() fractional_digits:("." hex_digits())? {
            let fractional_digits = match fractional_digits {
                Some(digits) => digits_from_slice(&whole_digits, HEX_DIGITS),
                None => Vec::new(),
            };

            Number {
                whole_digits: digits_from_slice(&whole_digits, HEX_DIGITS),
                fractional_digits,
                base: Base::Binary,
                positive: negation_sign.is_none()
            }
        }

        rule binary_digits() -> Vec<char>
          = digits:['0'..='1']* { digits.to_vec() }

        rule binary_number() -> Number
          = negation_sign:"-"? "0b" whole_digits:binary_digits() fractional_digits:("." binary_digits())? {
            let fractional_digits = match fractional_digits {
                Some(digits) => digits_from_slice(&whole_digits, BINARY_DIGITS),
                None => Vec::new(),
            };

            Number {
                whole_digits: digits_from_slice(&whole_digits, BINARY_DIGITS),
                fractional_digits,
                base: Base::Binary,
                positive: negation_sign.is_none()
            }
        }

        rule denary_digits() -> Vec<char>
          = digits:['0'..='9']* { digits.to_vec() }

        rule denary_number() -> Number
          = negation_sign:"-"? whole_digits:denary_digits() fractional_digits:("." denary_digits())? {
            let fractional_digits = match fractional_digits {
                Some(digits) => digits_from_slice(&whole_digits, DENARY_DIGITS),
                None => Vec::new(),
            };

            Number {
                whole_digits: digits_from_slice(&whole_digits, DENARY_DIGITS),
                fractional_digits,
                base: Base::Denary,
                positive: negation_sign.is_none()
            }
        }

        // Matches number literals
        rule number() -> Number
          = number:binary_number() { number }
          / number:hex_number() { number }
          / number:denary_number() { number }

        /// Matches literals
        rule literal() -> Literal
          = value:number() {
            Literal::Number(value)
        }

        pub rule expression() -> Expression
         = precedence!{
            left:(@) _ "+" _ right:@ {
                Expression::Binary(
                    BinaryExpression { left: Box::new(left), right: Box::new(right), operator: BinaryOperator::Add }
                )
            }
            left:(@) _ "-" _ right:@ {
                Expression::Binary(
                    BinaryExpression { left: Box::new(left), right: Box::new(right), operator: BinaryOperator::Subtract }
                )
            }
            --
            left:(@) _ "*" _ right:@ {
                Expression::Binary(
                    BinaryExpression { left: Box::new(left), right: Box::new(right), operator: BinaryOperator::Multiply }
                )
            }
            left:(@) _ "/" _ right:@ {
                Expression::Binary(
                    BinaryExpression { left: Box::new(left), right: Box::new(right), operator: BinaryOperator::Divide }
                )
            }
            --
            left:@ _ "^" _ right:(@) {
                Expression::Binary(
                    BinaryExpression { left: Box::new(left), right: Box::new(right), operator: BinaryOperator::Exponent }
                )
            }
            --
            "-" _ expression:(@) {
                Expression::Unary(UnaryExpression { operand: Box::new(expression), operator: UnaryOperator::Negate })
            }
            --
            _ value:literal() _ {
                Expression::Literal(value)
            }
            _ "(" _ expression:expression() _ ")" _ { expression }
        }

        pub rule list() -> Vec<Expression>
          = "[" list:(expression() ** (_ "," _)) "]" { list }
    }
}

pub fn parse(input: &str) -> Expression {
    match m_parser::expression(input) {
        Ok(expression) => expression,
        Err(error) => panic!("Syntax Error: {}", error),
    }
}
