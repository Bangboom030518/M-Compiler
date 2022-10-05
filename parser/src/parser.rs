use crate::ast::{
    Base, BinaryExpression, BinaryOperator, CallExpression, Expression, Fractional, GenericParams,
    Integer, Literal, Namespace, NamespaceAccess, Number, Sign, Statement, Type, UnaryExpression,
    UnaryOperator,
};
use peg::parser;

// TODO: find a way to make parser modular.
parser! {
    pub grammar m_parser() for str {
        // Comments like this one
        rule line_comment() = "//" (!"\n" [_])* ("\n" / ![_])

        /* Comments like this */
        rule inline_comment() = "/*" (!"*/" [_])* "*/"

        rule _() = quiet!{ (" " / "\n" / "\t" / "\r\n" / inline_comment() / line_comment())* }

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
            let charcode = Base::Hexadecimal.parse_digits(digits).and_then(|number| {
              if let Ok(number) = number.try_into() {
                Some(number)
              } else {
                None
              }
            });
            charcode.map_or(DEFAULT, |charcode| char::from_u32(charcode).unwrap_or(DEFAULT))
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

        rule csv<T>(kind: rule<T>) -> Vec<T>
          = _ values:(kind() ** (_ ",")) _ ","? _ {
            values
          }

        rule list() -> Vec<Expression>
          = "[" expressions:csv(<expression()>) "]" {
            expressions
          }

        rule identifier_char()
          = [character if character.is_alphanumeric() || character == '_']

        rule identifier() -> String
          = quiet!{ identifier:$([character if character.is_alphabetic()] identifier_char()*) {
              identifier.to_string()
            } }
          / expected!("identifier")

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

        rule data_type() -> Type
          = precedence! {
            operand:@ "<" arguments:csv(<data_type()>) ">" {
              Type::GenericParams(GenericParams::new(operand, arguments))
            }
            --
            left:(@) "::" right:@ {
                Type::NamespaceAccess(NamespaceAccess::new(left, right))
            }
            --
            identifier:identifier() {
              Type::Identifier(identifier)
            }
          }

        // rule data_type_without_namespace_access() -> Type
        // = data_type:data_type() {?
        //   if let Type::NamespaceAccess(_) = data_type {
        //     Err("Oh dear, it's broken")
        //   } else {
        //     Ok(data_type)
        //   }
        // }
        //   = precedence! {
        //     operand:@ "<" arguments:csv(<data_type()>) ">" {
        //       Type::GenericParams(GenericParams::new(operand, arguments))
        //     }
        //     --
        //     identifier:identifier() {
        //       Type::Identifier(identifier)
        //     }
        //   }

        rule expression() -> Expression
         = precedence! {
            callable:(@) "(" arguments:csv(<expression()>) ")" {
              Expression::Call(CallExpression {
                callable: Box::new(callable),
                arguments,
                type_arguments: Vec::new()
              })
            }

            parent:data_type() "::" child:(@) {
              Expression::Namespace(Namespace {
                  parent: vec![parent],
                  child: Box::new(child)
              })
            }
            --
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
            
            _ identifier:identifier() _ {
                Expression::Identifier(identifier)
              }
        }

        rule statement() -> Statement
          = expression:expression() {
            Statement::Expression(expression)
          }

        pub rule body() -> Vec<Statement>
          = statements:(_ statement:statement() _ ";" { statement })* _ { statements }
    }
}
