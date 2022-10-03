use super::Expression as GenericExpression;
use crate::ast::types::Type;

#[derive(Debug)]
pub struct Call {
    pub callable: Box<GenericExpression>,
    pub arguments: Vec<GenericExpression>,
    pub type_arguments: Vec<Type>,
}
