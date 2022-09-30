use super::Expression as GenericExpression;

#[derive(Debug)]
pub enum Operator {
    Negate,
    Bang,
}

#[derive(Debug)]
pub struct Expression {
    pub operand: Box<GenericExpression>,
    pub operator: Operator,
}

impl Expression {
    pub fn new(operand: GenericExpression, operator: Operator) -> Self {
        Self {
            operand: Box::new(operand),
            operator,
        }
    }
}
