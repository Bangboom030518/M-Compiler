use rand_derive::Rand;

use super::Expression as GenericExpression;
use crate::ast::data_type::Type;

#[derive(Debug, Clone, PartialEq, Eq, Rand)]
#[exclude_test]
pub struct Call {
    pub callable: Box<GenericExpression>,
    pub arguments: Vec<GenericExpression>,
    pub type_arguments: Vec<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq, Rand)]
#[exclude_test]
pub struct Namespace {
    pub parent: Vec<Type>,
    pub child: Box<GenericExpression>,
}
