#![warn(clippy::pedantic, clippy::nursery)]

pub use ast::*;
pub use errors::ParseError;
use parser::m_parser;

mod ast;
mod errors;
mod parser;

/// Parses a string into the parse tree
///
/// # Errors
/// Returns a formatted error if the parser is unable to parse a string due to a user syntax error
pub fn parse(input: &str) -> Result<Vec<Statement>, ParseError> {
    m_parser::body(input).map_err(|error| ParseError::new(error, input))
}
