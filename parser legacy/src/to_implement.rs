// TODO: add spans to everything

use crate::ast::{data_type, declaration, expression, pattern, Expression, Identifier, Statement, Type};
use expression::literal::{number, Number};
use expression::Literal;
use peg::parser;
use span::Span;

const KEYWORDS: &[&str] = &[
    "var", "let", "const", "static", "while", "for", "type", "struct", "enum", "trait", "import",
    "from", "as", "export", "public", "function", "super", "package", "return", "break",
    "continue",
];

// TODO: find a way to make parser modular.
parser! {
    pub grammar m_parser() for str {
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
            _ "(" _ expression:expression() _ ")" _ { expression }

            _ identifier:identifier() _ {
                Expression::Identifier(identifier)
              }
        }

        rule continue_statement() -> ()
          = "continue" {}

        rule break_statement() -> ()
          = "break" {}

        rule statement() -> Statement
          = expression:expression() {
            Statement::Expression(expression)
          }
          / continue_statement() {
            Statement::Continue
          }
          / break_statement() {
            Statement::Break
          }

        rule body() -> Vec<Statement>
          = statements:(_ statement:statement() _ ";" { statement })* _ { statements }

        rule import() -> declaration::top_level::Import
          = "import" __  path:(identifier() ++ "::") _ ";" {
            declaration::top_level::Import { path }
          }

        rule type_parameters() -> Vec<Identifier>
          = "<" _ identifiers:csv(<identifier()>) _ ">" {
            identifiers
          }

        rule pattern() -> pattern::Pattern
          = identifier:identifier() {
            pattern::Pattern::Identifier(identifier)
          }
        
        rule parameter() -> declaration::top_level::Parameter
          = pattern:pattern() _ ":" _ data_type:data_type() {
            declaration::top_level::Parameter { pattern, data_type }
          }

        rule parameters() -> Vec<declaration::top_level::Parameter>
          = "(" _ params:csv(<parameter()>) _ ")" {
            params
          }

        rule function() -> declaration::top_level::Function
          = "function" __ type_parameters:type_parameters()? _ parameters:parameters() _ body:expression() {
            declaration::top_level::Function {
              type_parameters,
              parameters
            }
          }

        rule top_level_declaration() -> declaration::top_level::Declaration
          = import:import() {
              declaration::top_level::Declaration::Import(import)
            }

            pub rule program() -> Vec<declaration::top_level::Declaration>
          = _ declarations:(top_level_declaration() ** _) _ {
            declarations
          }

    }
}
