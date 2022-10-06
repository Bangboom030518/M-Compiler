use crate::ast::{
    declaration::Import, Base, BinaryExpression, BinaryOperator, CallExpression, Declaration,
    Expression, Fractional, GenericParams, Integer, Literal, Namespace, Number, Sign, Statement,
    Type, UnaryExpression, UnaryOperator,
};
use peg::parser;

const KEYWORDS: &[&str] = &[
    "var", "let", "const", "static", "while", "for", "type", "struct", "enum", "trait", "import",
    "from", "as", "export", "public",
];

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
            Base::Hexadecimal.parse_digits(digits)
              .and_then(|number| number.try_into().ok())
              .and_then(char::from_u32).unwrap_or(DEFAULT)
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
          = values:(_ kind:kind() ** (_ ",") { kind }) (_ ",")? {
            values
          }

        rule import() -> Import
          = "import " namespaces:(_ identifiers:identifier() ++ (_ ",") { identifiers }) " from " path:string() {
            Import { path, namespaces }
          }

        rule declaration() -> Declaration
         = import:import() {
            Declaration::Import(import)
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
              Type::GenericParams(GenericParams::new(operand, arguments))
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
              Expression::Call(CallExpression {
                callable: Box::new(callable),
                arguments,
                type_arguments: Vec::new()
              })
            }

            parent:data_type_with_terminating_expression() "::" child:(@) {
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
          = declaration:declaration() {
            Statement::Declaration(declaration)
          }
          / expression:expression() {
            Statement::Expression(expression)
          }

        pub rule body() -> Vec<Statement>
          = statements:(_ statement:statement() _ ";" { statement })* _ { statements }
    }
}
