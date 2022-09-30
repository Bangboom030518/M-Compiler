#![warn(clippy::pedantic, clippy::nursery)]

use errors::CompilerError;
use ast::Statement;
use parser::m_parser;

mod parser;
mod ast;
mod errors;

#[must_use]
pub fn parse(input: &str) -> Vec<Statement> {
    match m_parser::body(input) {
        Ok(body) => body,
        Err(error) => {
            let err = CompilerError::parse_error(error, input);
            eprintln!("{}", err);
            std::process::exit(1);
        }
    }
}
