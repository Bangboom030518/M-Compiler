use crate::Expression as GenericExpression;
pub use expression::Expression;
pub use operator::Operator;
pub use terms::Terms;

pub fn fmt_expression(
    f: &mut std::fmt::Formatter,
    expression: &GenericExpression,
) -> std::fmt::Result {
    match expression {
        GenericExpression::If(_)
        | GenericExpression::Return(_)
        | GenericExpression::While(_)
        | GenericExpression::Binary(_) => {
            write!(f, "({expression})")
        }
        _ => write!(f, "{expression}"),
    }
}

mod expression;
mod operator;
mod term;
mod terms;
