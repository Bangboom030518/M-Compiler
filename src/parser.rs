use crate::utils;
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
    type_annotation: Token ,
    kind: VariableDeclarationKind,
    value: Expression
}

struct Tokens {
    tokens: Vec<Token>,
    current: usize
}

impl Tokens {
    // fn next() -> Token {

    // }

    fn new(tokens: &[Token]) -> Self {
        Self { tokens: tokens.to_vec(), current: 0 }
    }
}

pub fn parse(tokens: &[Token]) {
    let mut tokens = tokens.into_iter();
    loop {
        break;
    }
    while let Some(token) = tokens.next() {
        match token {
            
            Token::Keyword(keyword) => {
                match *keyword {
                    Keyword::Const => {
                        if let Some(Token::Identifier(identifier)) = tokens.next() {

                        } else {
                            utils::error("`const` keyword requires an identifier.")
                        }
                    }
                    _ => {
                    }
                }
            }
            token => {
                utils::error(&format!("Unexpected token: {:?}", token));
            }
        }

    }
}
