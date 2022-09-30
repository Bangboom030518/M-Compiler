// TODO: find a way to make parser modular.
use errors::CompilerError;
use peg::parser;
use std::fmt;

mod errors;

const BINARY_DIGITS: &[char] = &['0', '1'];
const DENARY_DIGITS: &[char] = &['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
const HEX_DIGITS: &[char] = &[
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f',
];

#[derive(Debug)]
pub enum Literal {
    Number(Number),
    List(Vec<Expression>),
    Char(char),
}

#[derive(Debug)]
pub enum Base {
    Binary = 2,
    Octal = 8,
    Denary = 10,
    Hexadecimal = 16,
}

impl Base {
    const BINARY_DIGITS: &[char] = &['0', '1'];
    const DENARY_DIGITS: &[char] = &['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
    const OCTAL_DIGITS: &[char] = &['0', '1', '2', '3', '4', '5', '6', '7'];
    const HEXADECIMAL_DIGITS: &[char] = &[
        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f',
    ];

    pub const fn get_digits(&self) -> &[char] {
        match self {
            Self::Binary => Self::BINARY_DIGITS,
            Self::Denary => Self::DENARY_DIGITS,
            Self::Octal => Self::OCTAL_DIGITS,
            Self::Hexadecimal => Self::HEXADECIMAL_DIGITS,
        }
    }

    fn as_number(&self) -> u8 {
        *self as u8
    }
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
            16 => Ok(Self::Hexadecimal),
            number => Err(number),
        }
    }
}

#[derive(Debug)]
pub enum Number {
    Integer(Integer),
    Fractional(Fractional),
}

#[derive(Debug)]
pub struct Integer {
    pub sign: Sign,
    pub digits: Vec<u8>,
    pub base: Base,
}

/// Fractional
#[derive(Debug)]
pub struct Fractional {
    pub sign: Sign,
    pub whole_digits: Vec<u8>,
    pub fractional_digits: Vec<u8>,
    pub base: Base,
}

#[derive(Debug)]
pub enum Sign {
    Negative,
    Positive,
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
    Bang,
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

        // Matches a digit given a base
        rule digit(base: &Base) -> u8
          = digit:quiet!{ [digit if base.get_digits().contains(&digit.to_ascii_lowercase())] } {
                let digits = base.get_digits();
                digits.iter().position(|&character| character == digit.to_ascii_lowercase()).unwrap_or_else(|| {
                    panic!(
                        "Digit in base {} should not be {}",
                        digits.len(),
                        digit
                    )
                }) as u8
            }
          / expected!("digit")

        rule base() -> Base
          = "0b" { Base::Binary }
          / "0o" { Base::Octal }
          / "0x" { Base::Hexadecimal }
          / "" { Base::Denary }

        rule sign() -> Sign
          = "-" { Sign::Negative }
          / "" { Sign::Positive }

        rule digits(base: &Base) -> Vec<u8>
          = digits:digit(base) ++ ("_"?) { digits }

        rule number() -> Number
          = sign:sign() base:base() whole_digits:digits(&base)? "." fractional_digits:digits(&base) {
                let whole_digits = whole_digits.unwrap_or_default();
                Number::Fractional(Fractional {
                    sign,
                    whole_digits,
                    fractional_digits,
                    base
                })
            }
          / sign:sign() base:base() digits:digits(&base) {
                Number::Integer(Integer {
                    digits,
                    base,
                    sign
                })
            }


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
            left:@ _ "**" _ right:(@) {
                Expression::Binary(
                    BinaryExpression { left: Box::new(left), right: Box::new(right), operator: BinaryOperator::Exponent }
                )
            }
            --
            "-" _ expression:(@) {
                Expression::Unary(UnaryExpression { operand: Box::new(expression), operator: UnaryOperator::Negate })
            }
            "!" _ expression:(@) {
                Expression::Unary(UnaryExpression { operand: Box::new(expression), operator: UnaryOperator::Bang })
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
        Err(error) => {
            let err = CompilerError::parse_error(error, input);
            eprintln!("{}", err);
            std::process::exit(1);
        }
    }
}
