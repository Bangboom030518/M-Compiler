use crate::Expression;

#[derive(Debug, Clone)]
pub enum Declaration {
    Import(Import),
    Variable(Variable),
}

#[derive(Debug, Clone)]
pub struct Import {
    pub path: Vec<String>,
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
