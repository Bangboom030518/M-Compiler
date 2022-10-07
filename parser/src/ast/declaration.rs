use super::Expression;

#[derive(Debug)]
pub enum Declaration {
    Import(Import),
    Variable(Variable),
}

#[derive(Debug)]
pub struct Import {
    pub path: String,
    pub namespaces: Vec<String>,
}

#[derive(Debug)]
pub struct Variable {
    pub kind: VariableKind,
    pub value: Expression,
}

#[derive(Debug)]
pub enum VariableKind {
    Static,
    Let,
    Const,
    Var,
}