use crate::prelude::*;
use rand::prelude::*;

#[derive(Debug, Clone, PartialEq, Eq, Rand)]
pub struct Identifier(pub String);

impl Identifier {
    /// Parse a valid first character for an `M` identifier.
    ///
    /// # Examples
    ///
    /// ```
    /// assert_eq!(Self::first_character("a"), Ok("", 'a'));
    /// assert_eq!(Self::first_character("_"), Ok("", '_'));
    /// assert_eq!(character("&"), Err(nom::Err::Error(("&", nom::error::ErrorKind::Satisfy))));
    /// assert_eq!(character("1"), Err(nom::Err::Error(("1", nom::error::ErrorKind::Satisfy))));
    /// ```
    fn first_character(input: &str) -> IResult<char> {
        satisfy(|character| character == '_' || character.is_alphabetic())(input)
    }

    /// Parse a valid character for an `M` identifier
    ///
    /// # Examples
    ///
    /// ```
    /// assert_eq!(character("_"), Ok(("_", '_')));
    /// assert_eq!(character("a"), Ok(("", 'a')));
    /// assert_eq!(character("1"), Ok(("", '1')));
    /// assert_eq!(character("&"), Err(nom::Err::Error(("&", nom::error::ErrorKind::Satisfy))));
    /// ```
    fn character(input: &str) -> IResult<char> {
        satisfy(|character| character == '_' || character.is_alphanumeric())(input)
    }
}

impl std::fmt::Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::hash::Hash for Identifier {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

impl Parse for Identifier {
    fn parse(input: &str) -> IResult<Self> {
        map(
            pair(Self::first_character, many0(Self::character)),
            |(first_char, mut rest)| {
                rest.insert(0, first_char);
                Self(rest.into_iter().collect())
            },
        )(input)
    }
}

lazy_static::lazy_static! {
    static ref LOWERCASE_CHARS: Vec<char> = ('a'..='z').collect();
    static ref UPPERCASE_CHARS: Vec<char> = ('A'..='Z').collect();
    static ref NUMERIC_CHARS: Vec<char> = ('0'..='9').collect();
    static ref VALID_FIRST_CHARS: Vec<char> =
        [LOWERCASE_CHARS.clone(), UPPERCASE_CHARS.clone(), vec!['_']]
            .into_iter()
            .flatten()
            .collect();
    static ref VALID_CHARS: Vec<char> = [VALID_FIRST_CHARS.clone(), NUMERIC_CHARS.clone()]
        .into_iter()
        .flatten()
        .collect();
}

fn gen_rand_identifier<R: Rng + ?Sized>(rng: &mut R) -> String {
    let length = (0..10).choose(rng).unwrap();
    let mut chars = vec![VALID_FIRST_CHARS.choose(rng).unwrap()];
    for _ in 0..length {
        chars.push(VALID_CHARS.choose(rng).unwrap());
    }
    chars.into_iter().collect()
}
