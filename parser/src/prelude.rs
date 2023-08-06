use nom::{
    branch, bytes::complete as bytes, character::complete as character, combinator, multi, sequence,
};
use rand::distributions::Standard;
use rand::prelude::*;
pub type IResult<'a, T, E = nom::error::VerboseError<&'a str>> = Result<(&'a str, T), nom::Err<E>>;
pub use crate::whitespace::{
    delimited as whitespace_delimited, optional as opt_whitespace, required as whitespace,
};

pub use branch::*;
pub use bytes::*;
pub use character::*;
pub use combinator::*;
pub use multi::*;
pub use nom::InputTake;
pub use rand_derive::Rand;
pub use sequence::*;

pub trait Parse: Sized {
    fn parse(input: &str) -> IResult<Self>;

    fn test()
    where
        Self: std::fmt::Display + std::fmt::Debug + PartialEq,
        Standard: Distribution<Self>,
    {
        let mut rng = thread_rng();
        for _ in 0..100 {
            let test_subject: Self = rng.gen();
            let test_string = test_subject.to_string();
            let parsed = Self::parse(&test_string);
            assert_eq!(parsed, Ok(("", test_subject)), "'{test_string}' failed!");
        }
    }
}

pub fn csv0<'a, T, F: FnMut(&'a str) -> IResult<'a, T> + 'a>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<'a, Vec<T>> {
    terminated(
        separated_list0(char(','), whitespace_delimited(inner)),
        opt(char(',')),
    )
}

pub fn csv1<'a, T, F: FnMut(&'a str) -> IResult<'a, T> + 'a>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<'a, Vec<T>> {
    terminated(
        separated_list1(char(','), whitespace_delimited(inner)),
        opt(char(',')),
    )
}

#[cfg(test)]
#[test]
fn test_csv() {
    let mut parser = csv0(char('a'));
    assert_eq!(parser(""), Ok(("", vec![])));
    assert_eq!(parser("b"), Ok(("b", vec![])));
    assert_eq!(parser("a"), Ok(("", vec!['a'])));
    assert_eq!(parser("ab"), Ok(("b", vec!['a'])));
    assert_eq!(parser("a,a,a"), Ok(("", vec!['a', 'a', 'a'])));
    assert_eq!(parser("a,a,a,"), Ok(("", vec!['a', 'a', 'a'])));
    assert_eq!(parser("a,a,ab"), Ok(("b", vec!['a', 'a', 'a'])));
}
