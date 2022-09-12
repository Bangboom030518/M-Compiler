#![allow(soft_unstable)]
#![cfg(test)]

use super::*;

#[bench]
fn parse_add() {
    let tokens = tokenize("1 + 1;").expect("Pest failed to parse the input");
    parse(tokens);

}
