// TODO: find a way to make parser modular.

use peg::parser;

const BINARY_DIGITS: &[char] = &['0', '1'];
const DENARY_DIGITS: &[char] = &['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];

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
    Hexidecimal
}

#[derive(Debug)]
pub struct Number {
    whole_digits: Vec<u8>,
    fractional_digits: Vec<u8>,
    base: Base,
    positive: bool
}

#[derive(Debug)]
pub enum BinaryOperator {
    Multiply,
    Add,
    Subtract,
    Divide,
    Exponent
}

#[derive(Debug)]
pub struct BinaryExpression {
    left: Box<Expression>,
    right: Box<Expression>,
    operator: BinaryOperator,
}

#[derive(Debug)]
pub enum UnaryOperator {
    Negate
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
    Unary(UnaryExpression)
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
                        "Digit in base {} should not be {}",
                        base_digits.len(),
                        digit
                    )
                }) as u8
        })
        .collect()
}


parser! {
    grammar m_parser() for str {
        rule whitespace() = [' ' | '\n' | '\t']

        // Comments like this one
        rule line_comment() = "//" (!"\n" [_])* ("\n" / ![_])

        /* Comments like this */
        rule inline_comment() = "/*" (!"*/" [_])* "*/"

        rule _() = quiet!{ (whitespace() / "\n" / inline_comment() / line_comment())* }

        rule char() -> char
          = character:['a'] {
            character
          }

        /// Matches number literals
        rule number() -> Number
          = negation_sign:"-"? "0b" whole_digits:(['0'..='1']*) "." fractional_digits:(['0'..='1']*) {
            // Binary floats
            Number {
                whole_digits: digits_from_slice(&whole_digits, BINARY_DIGITS),
                fractional_digits: digits_from_slice(&fractional_digits, BINARY_DIGITS),
                base: Base::Binary,
                positive: negation_sign.is_none()
            }
        } / negation_sign:"-"? "0b" whole_digits:(['0'..='1']+) {
            // Binary ints
            Number {
                whole_digits: digits_from_slice(&whole_digits, BINARY_DIGITS),
                fractional_digits: Vec::new(),
                base: Base::Binary,
                positive: negation_sign.is_none()
            }
        } / negation_sign:"-"? whole_digits:(['0'..='9']*) "." fractional_digits:(['0'..='9']*) {
            // Denary floats
            Number {
                whole_digits: digits_from_slice(&whole_digits, DENARY_DIGITS),
                fractional_digits: digits_from_slice(&fractional_digits, DENARY_DIGITS),
                base: Base::Denary,
                positive: negation_sign.is_none()
            }
        } / negation_sign:"-"? whole_digits:(['0'..='9']+) {
            // Denary ints
            Number {
                whole_digits: digits_from_slice(&whole_digits, DENARY_DIGITS),
                fractional_digits: Vec::new(),
                base: Base::Denary,
                positive: negation_sign.is_none()
            }
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
        Err(error) => panic!("Syntax Error: {}", error)
    }
}
