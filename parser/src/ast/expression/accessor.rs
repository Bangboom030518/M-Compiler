use super::Expression as GenericExpression;
use crate::ast::types::Type;

#[derive(Debug, Clone)]
pub struct Call {
    pub callable: Box<GenericExpression>,
    pub arguments: Vec<GenericExpression>,
    pub type_arguments: Vec<Type>,
}

#[derive(Debug, Clone)]
pub struct Namespace {
    pub parent: Vec<Type>,
    pub child: Box<GenericExpression>,
}
