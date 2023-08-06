#![warn(clippy::pedantic, clippy::nursery)]
#![feature(iter_intersperse)]

pub mod ast;
mod prelude;
mod whitespace;

use ast::prelude::*;
use prelude::*;
use rand::prelude::*;

fn gen_rand_vec<R: rand::Rng + ?Sized, T, F: FnMut(&mut R) -> T>(
    rng: &mut R,
    mut generate: F,
) -> Vec<T> {
    let length = (0..3).choose(rng).unwrap();
    let mut result = Vec::with_capacity(length);
    for _ in 0..length {
        result.push(generate(rng));
    }
    result
}

fn gen_rand_string<R: rand::Rng + ?Sized>(rng: &mut R) -> String {
    let chars: Vec<char> = gen_rand_vec(rng, rand::Rng::gen);
    chars.into_iter().collect()
}

// TODO: finalise `parse`, to avoid `parse_functions` 

/// # Errors
/// - if the input given is unparseable
pub fn parse(input: &str) -> IResult<Vec<Statement>> {
    many0(terminated(
        map(
            whitespace_delimited(Expression::parse),
            Statement::Expression,
        ),
        char(';'),
    ))(input)
}

/// # Errors
/// - if the input given is unparseable
pub fn parse_functions(input: &str) -> IResult<Vec<Function>> {
    many0(terminated(
        whitespace_delimited(Function::parse),
        char(';'),
    ))(input)
}
