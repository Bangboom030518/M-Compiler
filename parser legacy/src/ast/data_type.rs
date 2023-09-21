// TODO: function type

use crate::prelude::*;
use super::Identifier;

#[derive(Debug, Clone, PartialEq, Eq, Rand)]
#[exclude_test]
pub struct Params {
    pub operand: Box<Type>,
    pub arguments: Vec<Type>,
}

impl Params {
    #[must_use]
    pub fn new(operand: Type, arguments: &[Type]) -> Self {
        Self {
            operand: Box::new(operand),
            arguments: Vec::from(arguments),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Rand)]
#[exclude_test]
pub enum Type {
    Identifier(Identifier),
    Params(Params),
    NamespaceAccess(Vec<Type>),
}

