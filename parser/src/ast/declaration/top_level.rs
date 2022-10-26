use crate::{pattern::Pattern, Expression, Identifier, Type};

#[derive(Debug, Clone)]
pub enum Declaration {
    Import(Import),
    Variable(Variable),
}

#[derive(Debug, Clone)]
pub struct Import {
    pub path: Vec<Identifier>,
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub kind: VariableKind,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub enum VariableKind {
    Static,
    Const,
}

pub struct Parameter {
    pattern: Pattern,
    data_type: Type,
}

pub struct TypeBound {
    data_type: Type,
    // TODO: not just traits lolz
    trait_bound: Type,
}

pub struct Function {
    type_parameters: Vec<Identifier>,
    type_bounds: Vec<TypeBound>,
    parameters: Vec<Parameter>,
    body: Expression,
}
