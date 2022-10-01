use crate::ast::{
    Base, BinaryExpression, BinaryOperator, Expression, Fractional, Integer, Literal, Number, Sign,
    Statement, UnaryExpression, UnaryOperator,
};
use peg::parser;

// TODO: find a way to make parser modular.
parser! {
    pub grammar m_parser() for str {
        rule whitespace() =  [' ' | '\n' | '\t']

        // Comments like this one
        rule line_comment() = "//" (!"\n" [_])* ("\n" / ![_])

        /* Comments like this */
        rule inline_comment() = "/*" (!"*/" [_])* "*/"

        rule _() = quiet!{ (whitespace() / "\n" / inline_comment() / line_comment())* }

        rule char() -> char
          = r"\n" { '\n' }
          / r"\r" { '\r' }
          / r"\t" { '\t' }
          / r"\\" { '\\' }
          / r"\u{" digits:digit(&Base::Hexadecimal)*<4> { 
            // TODO: convert digits to number and get char at that codepoint
            '😊'
          }
          / "\"" { '\"' }
          / character:[_] {
            character
          }

        /// Matches a digit given a base
        rule digit(base: &Base) -> usize
          = digit:quiet!{ [digit if base.get_digits().contains(&digit.to_ascii_lowercase())] } {
                let digits = base.get_digits();
                digits.iter().position(|&character| character == digit.to_ascii_lowercase()).unwrap_or_else(|| {
                    panic!(
                        "Digit in base {} should not be {}",
                        digits.len(),
                        digit
                    )
                })
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

        rule digits(base: &Base) -> Vec<usize>
          = digits:digit(base) ++ ("_"?) { digits }

        rule integer() -> Integer
          = quiet!{
                sign:sign() base:base() digits:digits(&base) {
                    Integer {
                        digits,
                        base,
                        sign
                    }
                }
            }
          / expected!("integer")

        rule fractional() -> Fractional
          = quiet!{
                sign:sign() base:base() whole_digits:digits(&base)? "." fractional_digits:digits(&base) {
                    let whole_digits = whole_digits.unwrap_or_default();
                    Fractional {
                        sign,
                        whole_digits,
                        fractional_digits,
                        base
                    }
                }
            }
          / expected!("float")


        rule number() -> Number
          = fractional:fractional() {
            Number::Fractional(fractional)
          }
          / integer:integer() {
            Number::Integer(integer)
          }

        /// Matches literals
        rule literal() -> Literal
          = value:number() {
            Literal::Number(value)
        }

        rule binary_operator_l1() -> BinaryOperator
          = "+" { BinaryOperator::Add }
          / "-" { BinaryOperator::Subtract }

        rule expression() -> Expression
         = precedence!{
            left:(@) _ operator:(
                  "+" { BinaryOperator::Add }
                / "-" { BinaryOperator::Subtract }
            ) _ right:@ {
                Expression::Binary(
                    BinaryExpression::new(left, right, operator)
                )
            }
            --
            left:(@) _ operator:(
                "*" { BinaryOperator::Multiply }
              / "/" { BinaryOperator::Divide }
            ) _ right:@ {
                Expression::Binary(
                    BinaryExpression::new(left, right, operator)
                )
            }
            --
            left:@ _ "**" _ right:(@) {
                Expression::Binary(
                    BinaryExpression::new(left, right, BinaryOperator::Exponent)
                )
            }
            --
            operator:(
                "-" { UnaryOperator::Negate }
              / "!" { UnaryOperator::Bang }
            ) _ operand:(@) {
                Expression::Unary(UnaryExpression::new(operand, operator))
            }
            --
            _ value:literal() _ {
                Expression::Literal(value)
            }
            _ "(" _ expression:expression() _ ")" _ { expression }
        }

        rule statement() -> Statement
          = expression:expression() {
            Statement::Expression(expression)
          }

        pub rule body() -> Vec<Statement>
          = statements:(_ statement:statement() _ ";" { statement })* { statements }
    }
}
