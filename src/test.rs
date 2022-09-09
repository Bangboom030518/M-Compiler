#![cfg(test)]
use super::*;
use parser::{Program, Statement};
use parser::expressions::{Expression, Literal, BinaryExpression, BinaryOperator, UnaryExpression, UnaryOperator};

const fn number(number: f64) -> Expression {
    Expression::Literal(Literal::Number(number))
}

#[test]
fn parse_add() {
    let tokens = tokenize("1 + 1;").expect("Pest failed to parse the input");
    let tree = parse(tokens);
    assert_eq!(
        tree,
        Program {
            body: vec![Statement::Expression(Expression::Binary(
                BinaryExpression {
                    left: Box::new(number(1.0)),
                    right: Box::new(number(1.0)),
                    operator: BinaryOperator::Addition
                }
            ))]
        }
    );
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
    );
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
    );
}

#[test]
fn parse_group() {
    let tokens = tokenize("((((((1))))));").expect("Pest failed to parse the input");
    let tree = parse(tokens);
    assert_eq!(
        tree,
        Program {
            body: vec![Statement::Expression(number(1.0))]
        }
    );
}

#[test]
fn parse_operator_precedance() {
    let tokens = tokenize("1 + 2 * 3 + 4;").expect("Pest failed to parse the input");
    let tree = parse(tokens);
    assert_eq!(
        tree,
        Program {
            body: vec![Statement::Expression(Expression::Binary(
                BinaryExpression {
                    left: Box::new(Expression::Binary(
                        BinaryExpression {
                            left: Box::new(number(1.0)),
                            right: Box::new(Expression::Binary(
                                BinaryExpression {
                                    left: Box::new(number(2.0)),
                                    right: Box::new(number(3.0)),
                                    operator: BinaryOperator::Multiplication
                                }
                            )),
                            operator: BinaryOperator::Addition
                        }
                    )),
                    right: Box::new(number(4.0)),
                    operator: BinaryOperator::Addition
                }
            ))]
        }
    );

}
