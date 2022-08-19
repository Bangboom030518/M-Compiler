use core::panic;

use crate::tokenizer::{Token, Keyword};

enum VariableDeclarationKind {
    Const,
    Let,
}

enum Literal {
    String(String),
    Integer(usize),
    Char(char),
    Decimal(f64),
}

enum BinaryOperator {
    Addition,
    Subtraction,
    Division,
    Multiplication
}

enum UnaryOperator {
    Bang
}

struct BinaryExpression {
    left: Box<Expression>,
    right: Box<Expression>,
    operator: BinaryOperator
}

struct UnaryExpression {
    operand: Box<Expression>,
    operator: UnaryOperator,
}

enum Expression {
    Literal(Literal),
    BinaryExpression(BinaryExpression),
    UnaryExpression(UnaryExpression)
}

struct VariableDeclaration {
    type_annotation: Token,
    kind: VariableDeclarationKind,
    value: Expression
}

pub fn parse(tokens: &[Token]) -> AST {
    let mut tokens = tokens.into_iter();
    while let Some(token) = tokens.next() {
        match token {
            Token::Keyword(Keyword::Const) => {
                if let Some(Token::Identifier(identifier)) = tokens.next() {

                } else {
                    panic!("`const` keyword requires an identifier.")
                }
            }
            _ => {}
        }

    }
}
