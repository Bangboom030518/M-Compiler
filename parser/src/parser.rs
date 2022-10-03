use crate::ast::{
    Base, BinaryExpression, BinaryOperator, CallExpression, Expression, Fractional, Integer,
    Literal, Number, Sign, Statement, UnaryExpression, UnaryOperator,
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
          = "'" character:string_char() "'" { character }

        rule string() -> String
          = "\"" chars:(!['"'] character:string_char() { character })* "\"" {
              chars.into_iter().collect()
            }

        rule string_char() -> char
          = r"\n" { '\n' }
          / r"\r" { '\r' }
          / r"\t" { '\t' }
          / r"\\" { '\\' }
          / r"\0" { '\0' }
          / r"\u{" digits:(digit(&Base::Hexadecimal))*<1,6> "}" {
            const DEFAULT: char = '�';
            let charcode: u32 = match Base::Hexadecimal.parse_digits(digits).try_into() {
              Ok(number) => number,
              Err(_) => return DEFAULT
            };
            char::from_u32(charcode).unwrap_or(DEFAULT)
          }
          / r#"\""# { '"' }
          / r"\'" { '\'' }
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

        rule list() -> Vec<Expression>
          = "[" _ expressions:(expression() ** (_ ",")) _ "]" {
            expressions
          }

        /// Matches literals
        rule literal() -> Literal
          = value:number() {
              Literal::Number(value)
            }
          / character:char() {
              Literal::Char(character)
            }
          / string:string() {
              Literal::String(string)
            }
          / list:list() {
              Literal::List(list)
            }

        rule binary_operator_l1() -> BinaryOperator
          = "+" { BinaryOperator::Add }
          / "-" { BinaryOperator::Subtract }

        rule expression() -> Expression
         = precedence! {
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
            callable:(@) "(" _ arguments:expression() ** (_ ",") ")" {
                Expression::Call(CallExpression {
                  callable: Box::new(callable),
                  arguments,
                  type_arguments: Vec::new()
                })
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
