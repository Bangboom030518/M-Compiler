use super::Expression as GenericExpression;
use crate::ast::types::Type;

#[derive(Debug)]
pub struct Call {
    pub callable: Box<GenericExpression>,
    pub arguments: Vec<GenericExpression>,
    pub type_arguments: Vec<Type>,
}

#[derive(Debug)]
pub struct Namespace {
    pub parent: Vec<Type>,
    pub child: Box<GenericExpression>,
}
