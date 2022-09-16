#![cfg(test)]

use super::super::{Program, Statement};
use crate::{tokenize, parse, error};

#[test]
fn tokenize_var_declaration() {
    match tokenize("let _: _ = \"Hello World\"") {
        Ok(tokens) => tokens,
        Err(message) => panic!("Parse Error {}", message),
    };
}