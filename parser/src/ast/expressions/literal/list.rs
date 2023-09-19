use crate::prelude::*;
use crate::Expression;
use rand_derive::Rand;

#[derive(Debug, Clone, PartialEq, Eq, Rand)]
pub struct List {
    list: Vec<Expression>,
}

impl Parse for List {
    fn parse(input: &str) -> IResult<Self> {
        map(
            delimited(
                char('['),
                csv0(preceded(peek(none_of("]")), Expression::parse)),
                char(']'),
            ),
            Self::new,
        )(input)
    }
}

impl List {
    pub fn new(list: Vec<Expression>) -> Self {
        Self { list }
    }
}

impl std::fmt::Display for List {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "[{}]",
            self.list
                .iter()
                .map(ToString::to_string)
                .intersperse(",".to_string())
                .collect::<String>()
        )
    }
}
