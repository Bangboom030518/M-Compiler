use crate::{
    ast::{expressions::Expression, Identifier},
    prelude::*,
};

#[derive(Debug, Clone, PartialEq, Eq, Rand)]
pub struct Function {
    pub name: Identifier,
    pub parameters: Vec<Identifier>,
    pub return_variable: Identifier,
    pub statements: Vec<Expression>,
}


impl Function {
    #[must_use]
    pub fn new(
        name: Identifier,
        parameters: Vec<Identifier>,
        return_variable: Identifier,
        statements: Vec<Expression>,
    ) -> Self {
        Self {
            name,
            parameters,
            return_variable,
            statements,
        }
    }
}

impl Parse for Function {
    fn parse(input: &str) -> IResult<Self> {
        map(
            tuple((
                preceded(tag("function "), Identifier::parse),
                delimited(char('('), csv1(Identifier::parse), char(')')),
                preceded(tag(" -> "), Identifier::parse),
                preceded(tag("do "), many1(terminated(Expression::parse, newline))),
            )),
            |(name, parameters, return_variable, statements)| {
                Self::new(name, parameters, return_variable, statements)
            },
        )(input)
    }
}

impl std::fmt::Display for Function {
    fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // let Self {
        //     name,
        //     parameters,
        //     return_variable,
        // } = self;
        // write!(
        //     f,
        //     "function {name} ({}) do {return_value}",
        //     parameters
        //         .iter()
        //         .map(ToString::to_string)
        //         .intersperse(",".to_string())
        //         .collect::<String>()
        // )
        todo!()
    }
}
