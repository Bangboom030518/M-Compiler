use super::Expression as GenericExpression;

#[derive(Debug, Clone)]
pub enum Operator {
    Negate,
    Bang,
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub operand: Box<GenericExpression>,
    pub operator: Operator,
}

impl Expression {
    #[must_use]
    pub fn new(operand: GenericExpression, operator: Operator) -> Self {
        Self {
            operand: Box::new(operand),
            operator,
        }
    }
}
