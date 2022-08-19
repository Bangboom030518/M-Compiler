use core::panic;

use crate::tokenizer::{Token, Keyword};

// enum VariableDeclaration {
//     Const
// }

// struct VariableDeclarationStatement {
//     declaration_type: VariableDeclaration
// }

pub enum Literal

pub struct Literal {

}

#[derive(Debug)]
pub enum Node {
    IntegerExpression(usize),
    FloatExpression(f64),
    StringExpression(String),
    VariableExpression(String),
    ImportStatement()
}


#[derive(Debug)]
pub struct AST {
    pub body: Vec<Node>,
}

// fn parse_keyword(token: Token, tokens: &mut Token) -> Option<Token> {
//     if let Token::Keyword(token) = token {
        
//     } else {
//         None
//     }
// }

pub fn parse(tokens: &[Token]) -> AST {
    let mut tokens = tokens.into_iter();
    let mut body: Vec<Node> = Vec::new();
    while let Some(token) = tokens.next() {
        match token {
            Token::Integer(value) => body.push(Node::IntegerExpression(*value)),
            Token::Decimal(value) => body.push(Node::FloatExpression(*value)),
            Token::String(string) => body.push(Node::StringExpression(string.to_string())),
            Token::Keyword(Keyword::Const) => {
                if let Some(Token::Identifier(identifier)) = tokens.next() {

                } else {
                    panic!("`const` keyword requires an identifier.")
                }
            }
            _ => {}
        }

    }
    AST {
        body
    }
}
