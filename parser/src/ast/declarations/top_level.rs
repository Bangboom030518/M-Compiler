use crate::ast::{Expression, Identifier, Pattern, Type};
use crate::prelude::*;
pub use function::Function;

mod function;

#[derive(Debug, Clone, PartialEq, Eq, Rand)]
#[exclude_test]
pub enum Declaration {
    Import(Import),
    Variable(Variable),
    Function(Function)
}

#[derive(Debug, Clone, PartialEq, Eq, Rand)]
#[exclude_test]
pub struct Import {
    pub path: Vec<Identifier>,
}

#[derive(Debug, Clone, PartialEq, Eq, Rand)]
#[exclude_test]
pub struct Variable {
    pub kind: VariableKind,
    pub value: Expression,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Rand)]
pub enum VariableKind {
    Static,
    Const,
}

impl std::fmt::Display for VariableKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let string = match self {
            Self::Static => "static",
            Self::Const => "const",
        };
        write!(f, "{string}")
    }
}

impl NomParse for VariableKind {
    fn parse(input: &str) -> IResult<Self> {
        alt((
            value(Self::Static, tag("static")),
            value(Self::Const, tag("const")),
        ))(input)
    }
}

/*

#[derive(Debug, Clone, PartialEq, Eq, Rand)]
#[exclude_test]
pub struct Parameter {
    pattern: Pattern,
    data_type: Type,
}

#[derive(Debug, Clone, PartialEq, Eq, Rand)]
#[exclude_test]
pub struct TypeBound {
    data_type: Type,
    // TODO: not just traits lolz
    trait_bound: Type,
}

#[derive(Debug, Clone, PartialEq, Eq, Rand)]
#[exclude_test]
pub struct Function {
    type_parameters: Vec<Identifier>,
    type_bounds: Vec<TypeBound>,
    parameters: Vec<Parameter>,
    body: Expression,
}
*/