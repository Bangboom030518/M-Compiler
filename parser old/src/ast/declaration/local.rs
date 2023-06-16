use crate::Expression;

#[derive(Debug, Clone)]
pub enum Declaration {
    Variable(Variable),
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub kind: VariableKind,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub enum VariableKind {
    Let,
    Var,
}
