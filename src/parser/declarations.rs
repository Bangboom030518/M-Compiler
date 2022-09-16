mod test; 

use crate::Pair;
use super::Expression;

#[derive(Debug, PartialEq, Clone)]
pub enum Declaration {
    Import(Import),
    Variable(Variable),
}

impl<'a> From<Pair<'a>> for Declaration {
    fn from(_: Pair<'a>) -> Self {
        unimplemented!("Declaration Parsing")
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Import {
    pub path: String,
    pub namespaces: Vec<String>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Variable {
    pub kind: VariableKind,
    pub value: Expression,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum VariableKind {
    Static,
    Let,
    Const,
    Var,
}

