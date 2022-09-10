use super::*;

#![cfg(test)]
#![allow(soft_unstable)]

#[bench]
fn parse_add() {
    let tokens = tokenize("1 + 1;").expect("Pest failed to parse the input");
    let tree = parse(tokens);
}
