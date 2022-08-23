use core::panic;

use super::*;

#[test]
fn add() {
    let tree = match parse("1 + 1;") {
        Ok(tree) => tree,
        Err(error) => panic!("Parse Error {}", error)
    };
    // assert_eq!(tree, 1)
}
