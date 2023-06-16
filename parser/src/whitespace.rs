use crate::prelude::{delimited as nom_delimited, *};

/// Parses a whitespace character
fn space(input: &str) -> IResult<()> {
    value((), space1)(input)
}

/// Parses a multiline comment in M (`/* ... */`)
fn multiline_comment(input: &str) -> IResult<()> {
    value((), tuple((tag("/*"), take_until("*/"), tag("*/"))))(input)
}

/// Parses a single-line comment in M (`// ...`)
fn single_line_comment(input: &str) -> IResult<()> {
    value((), pair(tag("//"), is_not("\n\r")))(input)
}

/// Parses a comment in M, either single-line (`// ...`) or multiline (`/* ... */`)
fn comment(input: &str) -> IResult<()> {
    alt((single_line_comment, multiline_comment))(input)
}

pub fn required(input: &str) -> IResult<()> {
    value((), many1(alt((space, comment))))(input)
}

pub fn optional(input: &str) -> IResult<()> {
    value((), opt(whitespace))(input)
}

pub fn delimited<'a, F: 'a + FnMut(&'a str) -> IResult<O>, O>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<O> {
    nom_delimited(optional, inner, optional)
}

#[test]
fn test() {
    assert_eq!(comment("// a"), Ok(("", ())));
    assert_eq!(comment("// a\nb"), Ok(("\nb", ())));
}
