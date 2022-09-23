#![cfg(test)]
use super::super::{Program, Statement};
use super::{
    BinaryExpression, BinaryOperator, Expression, Literal, UnaryExpression, UnaryOperator,
};
use rand::distributions::Uniform;
use crate::{parse, tokenize};
use rand::prelude::{thread_rng, Distribution};
use rand::seq::SliceRandom;

const fn number(number: f64) -> Expression {
    Expression::Literal(Literal::Number(number))
}

const BINARY_OPERATORS: &[&str] = &["*", "/", "+", "-", "%", "**", "&&", "||", "&", "^", "|"];

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
                    left: Box::new(Expression::Binary(BinaryExpression {
                        left: Box::new(number(1.0)),
                        right: Box::new(Expression::Binary(BinaryExpression {
                            left: Box::new(number(2.0)),
                            right: Box::new(number(3.0)),
                            operator: BinaryOperator::Multiplication
                        })),
                        operator: BinaryOperator::Addition
                    })),
                    right: Box::new(number(4.0)),
                    operator: BinaryOperator::Addition
                }
            ))]
        }
    );
}

#[test]
fn long_binary_expression() {
    let expression = generate_binary_expression(1000);
    let tokens = tokenize(&expression).unwrap_or_else(|err| panic!("{}", err));
    parse(tokens);
}

fn generate_binary_expression(length: usize) -> String {
    let mut rng = thread_rng();
    let uniform = Uniform::new(0, 1000);
    let mut result = format!("{}", uniform.sample(&mut rng));
    for _ in 0..length {
        let number = uniform.sample(&mut rng);
        let operator = BINARY_OPERATORS
            .choose(&mut rng)
            .expect("Failed to choose random item");
        result.push_str(&format!(" {} {}", operator, number));
    }
    format!("{};", result)
}

#[test]
fn complex_binary_expression() {
    let tokens = tokenize("!1 + !1;").expect("Pest failed to parse the input");
    let tree = parse(tokens);
    assert_eq!(
        tree,
        Program {
            body: vec![Statement::Expression(Expression::Binary(
                BinaryExpression {
                    left: Box::new(Expression::Unary(UnaryExpression {
                        operator: UnaryOperator::Bang,
                        operand: Box::new(number(1.0))
                    })),
                    right: Box::new(Expression::Unary(UnaryExpression {
                        operator: UnaryOperator::Bang,
                        operand: Box::new(number(1.0))
                    })),
                    operator: BinaryOperator::Addition
                }
            ))]
        }
    );
}
