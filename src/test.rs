use super::*;
#[cfg(test)]
use core::panic;
use parser::expressions::*;
use parser::*;

#[test]
fn parse_add() {
    let tokens = tokenize("1 + 1;").expect("Pest failed to parse the input");
    let tree = parse(tokens);
    assert_eq!(
        tree,
        Program {
            body: vec![Statement::Expression(Expression::Binary(
                BinaryExpression {
                    left: Box::new(Expression::Literal(Literal::Number(1.0))),
                    right: Box::new(Expression::Literal(Literal::Number(1.0))),
                    operator: BinaryOperator::Plus
                }
            ))]
        }
    )
}

#[test]
fn parse_bang() {
    let tokens = tokenize("!true;").expect("Pest failed to parse the input");
    let tree = parse(tokens);
    assert_eq!(
        tree,
        Program {
            body: vec![Statement::Expression(Expression::Unary(UnaryExpression {
                operator: UnaryOperator::Bang,
                operand: Box::new(Expression::Literal(Literal::Bool(true)))
            }))]
        }
    )
}

#[test]
fn parse_string() {
    let tokens = tokenize(r#""Hello World!";"#).expect("Pest failed to parse the input");
    let tree = parse(tokens);
    assert_eq!(
        tree,
        Program {
            body: vec![Statement::Expression(Expression::Literal(Literal::String(
                String::from("Hello World!")
            )))]
        }
    )
}

#[test]
fn parse_group() {
    let tokens = tokenize("((((((1))))));").expect("Pest failed to parse the input");
    let tree = parse(tokens);
    assert_eq!(
        tree,
        Program {
            body: vec![Statement::Expression(Expression::Literal(Literal::Number(
                1.0
            )))]
        }
    )
}
