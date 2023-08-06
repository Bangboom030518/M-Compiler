use crate::{prelude::*, Expression};

#[derive(Debug, Clone, PartialEq, Eq, Rand)]
pub struct While {
    condition: Box<Expression>,
    body: Box<Expression>,
}

impl Parse for While {
    fn parse(input: &str) -> IResult<Self> {
        map(
            pair(
                preceded(pair(tag("while"), whitespace), Expression::parse),
                preceded(
                    pair(whitespace, tag("do")),
                    preceded(whitespace, Expression::parse),
                ),
            ),
            Self::from,
        )(input)
    }
}

impl From<(Expression, Expression)> for While {
    fn from((condition, body): (Expression, Expression)) -> Self {
        Self {
            condition: Box::new(condition),
            body: Box::new(body),
        }
    }
}

impl std::fmt::Display for While {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let Self { condition, body } = self;
        write!(f, "while {condition} do {body}")
    }
}
