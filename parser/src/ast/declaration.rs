use super::expression::Expression;

#[derive(Debug, Clone)]
pub enum Declaration {
    Variable(LocalVariable),
}

#[derive(Debug, Clone)]
pub enum TopLevel {
    Import(Import),
    Variable(GlobalVariable),
}

#[derive(Debug, Clone)]
pub struct Import {
    pub path: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct LocalVariable {
    pub kind: LocalVariableKind,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub enum LocalVariableKind {
    Let,
    Var,
}

#[derive(Debug, Clone)]
pub struct GlobalVariable {
    pub kind: GlobalVariableKind,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub enum GlobalVariableKind {
    Static,
    Const,
}
