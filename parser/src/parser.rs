// TODO: add spans to everything

use crate::ast::{declaration, expression, data_type, Declaration, Expression, Statement, Type};
use expression::literal::{number, Number};
use expression::Literal;
use peg::parser;
use span::Span;

const KEYWORDS: &[&str] = &[
    "var", "let", "const", "static", "while", "for", "type", "struct", "enum", "trait", "import",
    "from", "as", "export", "public",
];

// TODO: find a way to make parser modular.
parser! {
    pub grammar m_parser() for str {
        /// Comments begining "//" and taking up a single line
        rule line_comment() = "//" (!"\n" [_])* ("\n" / ![_])

        /// Multiline comments beginning "/*" and ending "*/"
        rule inline_comment() = "/*" (!"*/" [_])* "*/"

        rule whitespace() = " " / "\n" / "\t" / "\r\n"

        /// Optional whitespace and comments
        rule _() = quiet!{ (whitespace() / inline_comment() / line_comment())* }

        /// Required whitespace or comment
        rule __() = quiet!{ (whitespace() / inline_comment() / line_comment())+ }

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
          / r"\u{" digits:(digit(&number::Base::Hexadecimal))*<1,6> "}" {
            const DEFAULT: char = '�';
            number::Base::Hexadecimal.parse_digits(digits)
              .and_then(|number| number.try_into().ok())
              .and_then(char::from_u32).unwrap_or(DEFAULT)
          }
          / r#"\""# { '"' }
          / r"\'" { '\'' }
          / character:[_] {
            character
          }

        /// Matches a digit given a base
        rule digit(base: &number::Base) -> usize
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

        rule base() -> number::Base
          = "0b" { number::Base::Binary }
          / "0o" { number::Base::Octal }
          / "0x" { number::Base::Hexadecimal }
          / "" { number::Base::Denary }

        rule sign() -> number::Sign
          = "-" { number::Sign::Negative }
          / "" { number::Sign::Positive }

        rule digits(base: &number::Base) -> Vec<usize>
          = digits:digit(base) ++ ("_"?) { digits }

        rule integer_type() -> number::integer::Type
          = "u8" { number::integer::Type::Unsigned8Bit }
          / "u16" { number::integer::Type::Unsigned16Bit }
          / "u32" { number::integer::Type::Unsigned32Bit }
          / "u64" { number::integer::Type::Unsigned64Bit }
          / "u128" { number::integer::Type::Unsigned128Bit }
          / "u128" { number::integer::Type::Unsigned128Bit }
          / "i8" { number::integer::Type::Signed8Bit }
          / "i16" { number::integer::Type::Signed16Bit }
          / "i32" { number::integer::Type::Signed32Bit }
          / "i64" { number::integer::Type::Signed64Bit }
          / "i128" { number::integer::Type::Signed128Bit }
          / "i128" { number::integer::Type::Signed128Bit }

        rule integer() -> number::Integer
          = quiet!{
                start:position!() sign:sign() base:base() digits:digits(&base) data_type:integer_type() end:position!() {
                    number::Integer {
                        base,
                        digits,
                        sign,
                        data_type,
                        span: Span {
                          start,
                          end,
                        }
                    }
                }
            }
          / expected!("integer")
        
        rule float_type() -> number::float::Type
          = "f32" { number::float::Type::F32Bit }
          / "f64" { number::float::Type::F64Bit }

        rule float() -> number::Float
          = quiet!{
                sign:sign() base:base() whole_digits:digits(&base)? "." fractional_digits:digits(&base) data_type:float_type() {
                    let whole_digits = whole_digits.unwrap_or_default();
                    number::Float {
                        sign,
                        whole_digits,
                        fractional_digits,
                        base,
                        data_type,
                        span: Span {
                          start: 0,
                          end: 0,
                        }
                    }
                }
            }
          / expected!("float")

        rule number() -> Number
          = float:float() {
            Number::Float(float)
          }
          / integer:integer() {
            Number::Integer(integer)
          }

        rule csv<T>(kind: rule<T>) -> Vec<T>
          = values:(_ kind:kind() ** (_ ",") { kind }) (_ ",")? {
            values
          }

        rule list() -> Vec<Expression>
          = "[" _ expressions:csv(<expression()>) _ "]" {
            expressions
          }

        rule identifier_char()
          = [character if character.is_alphanumeric() || character == '_']

        rule identifier() -> String
          = identifier:$(quiet! { [character if character.is_alphabetic()] identifier_char()* }) {? if KEYWORDS.contains(&identifier) {
              Err("non-reserved word")
            } else {
              Ok(identifier.to_string())
            }
          }
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

        rule data_type_term() -> Type
          = precedence! {
            operand:@ "<" _ arguments:csv(<data_type()>) _ ">" {
              Type::Params(data_type::Params::new(operand, arguments))
            }
            --
            identifier:identifier() {
              Type::Identifier(identifier)
            }
          }

        rule data_type() -> Type
          = left:data_type_term() terms:("::" data_type:data_type_term() { data_type })* {
            if terms.is_empty() {
              left
            } else {
              let mut terms = terms;
              terms.insert(0, left);
              Type::NamespaceAccess(terms)
            }
          }

        rule data_type_with_terminating_expression() -> Type
          = left:data_type_term() terms:("::" data_type:data_type_term() &("::" expression()) { data_type })* {
            if terms.is_empty() {
              left
            } else {
              let mut terms = terms;
              terms.insert(0, left);
              Type::NamespaceAccess(terms)
            }
          }

        rule expression() -> Expression
          = precedence! {
            callable:(@) "(" _ arguments:csv(<expression()>) _ ")" {
              Expression::Call(expression::accessor::Call {
                callable: Box::new(callable),
                arguments,
                type_arguments: Vec::new()
              })
            }

            parent:data_type_with_terminating_expression() "::" child:(@) {
              Expression::Namespace(expression::accessor::Namespace {
                  parent: vec![parent],
                  child: Box::new(child)
              })
            }
            --
            left:(@) _ operator:(
                  "+" { expression::binary::Operator::Add }
                / "-" { expression::binary::Operator::Subtract }
            ) _ right:@ {
                Expression::Binary(
                    expression::binary::Expression::new(left, right, operator)
                )
            }
            --
            left:(@) _ operator:(
                "*" { expression::binary::Operator::Multiply }
              / "/" { expression::binary::Operator::Divide }
            ) _ right:@ {
                Expression::Binary(
                    expression::binary::Expression::new(left, right, operator)
                )
            }
            --
            left:@ _ "**" _ right:(@) {
                Expression::Binary(
                    expression::binary::Expression::new(left, right, expression::binary::Operator::Exponent)
                )
            }
            --
            operator:(
                "-" { expression::unary::Operator::Negate }
              / "!" { expression::unary::Operator::Bang }
            ) _ operand:(@) {
                Expression::Unary(expression::unary::Expression::new(operand, operator))
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

        /// import std/net::tcp;
        /// import std/std;
        /// import extern;
        /// import module from "package";
        /// 
        /// 
        /// package
        ///   - mod_1
        ///   - mod_2
        /// 
        /// 
        /// 
        /// tcp::Server(|data| {
        ///   tcp::response(handle_request(data))
        /// })
        /// 
        /// std::math::sin()
        /// package
        /// 
        /// dependant:
        /// 
        /// import package/mod_1;
        /// 
        
        // rule namespace_csv() -> Vec<String>
        //   = namespaces:((_ identifier:identifier() { identifier }) ++ (_ ",")) { namespaces }
        rule import() -> declaration::Import
          = "import" __  package:(package:identifier() "/" { package }) module:identifier() {
            declaration::Import { path, namespaces }
          }


        rule top_level_declaration() -> declaration::TopLevel
          = import:import() {
            Declaration::Import(import)
         }

    }
}
