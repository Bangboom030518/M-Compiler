use super::expression::Expression;

#[derive(Debug, Clone)]
pub enum Declaration {
    Import(Import),
    Variable(Variable),
}

#[derive(Debug, Clone)]
pub struct Import {
    pub path: String,
    pub namespaces: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub kind: VariableKind,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub enum VariableKind {
    Static,
    Let,
    Const,
    Var,
}
