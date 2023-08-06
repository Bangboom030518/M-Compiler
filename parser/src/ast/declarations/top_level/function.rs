use crate::{
    ast::{expressions::Expression, Identifier},
    prelude::*,
};

#[derive(Debug, Clone, PartialEq, Eq, Rand)]
pub struct Function {
    pub name: Identifier,
    pub parameters: Vec<Identifier>,
    pub return_value: Expression,
}

impl Function {
    pub fn new(name: Identifier, parameters: Vec<Identifier>, return_value: Expression) -> Self {
        Self {
            name,
            parameters,
            return_value,
        }
    }
}

impl Parse for Function {
    fn parse(input: &str) -> IResult<Self> {
        map(
            tuple((
                preceded(tag("function "), Identifier::parse),
                delimited(char('('), csv1(Identifier::parse), char(')')),
                preceded(tag("do "), Expression::parse),
            )),
            |(name, parameters, return_value)| Self::new(name, parameters, return_value),
        )(input)
    }
}

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self {
            name,
            parameters,
            return_value,
        } = self;
        write!(
            f,
            "function {name} ({}) do {return_value}",
            parameters
                .iter()
                .map(ToString::to_string)
                .intersperse(",".to_string())
                .collect::<String>()
        )
    }
}
