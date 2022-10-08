use super::Expression as GenericExpression;

#[derive(Debug, Clone)]
pub enum Operator {
    Multiply,
    Add,
    Subtract,
    Divide,
    Exponent,
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub left: Box<GenericExpression>,
    pub right: Box<GenericExpression>,
    pub operator: Operator,
}

impl Expression {
    pub fn new(left: GenericExpression, right: GenericExpression, operator: Operator) -> Self {
        Self {
            left: Box::new(left),
            right: Box::new(right),
            operator,
        }
    }
}
