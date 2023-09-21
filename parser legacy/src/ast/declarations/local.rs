use crate::{prelude::*, Expression};
use rand_derive::Rand;

#[derive(Debug, Clone, PartialEq, Eq, Rand)]
#[exclude_test]
pub enum Declaration {
    Variable(Variable),
}

#[derive(Debug, Clone, PartialEq, Eq, Rand)]
#[exclude_test]
pub struct Variable {
    pub kind: VariableKind,
    pub value: Expression,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Rand)]
pub enum VariableKind {
    Let,
    Var,
}

impl std::fmt::Display for VariableKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let string = match self {
            Self::Let => "let",
            Self::Var => "var",
        };
        write!(f, "{string}")
    }
}

impl Parse for VariableKind {
    fn parse(input: &str) -> IResult<Self> {
        alt((value(Self::Let, tag("let")), value(Self::Var, tag("var"))))(input)
    }
}
