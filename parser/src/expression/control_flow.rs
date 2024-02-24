use tokenizer::TokenType;

use crate::internal::prelude::*;
use crate::{Error, Expression};

#[derive(Debug, Clone)]
pub struct If {
    pub condition: Box<Expression>,
    pub then_branch: Vec<Statement>,
    pub else_branch: Option<Vec<Statement>>,
    span: tokenizer::Span,
}

impl Parse for If {
    fn parse(parser: &mut Parser) -> Result<Self, Error> {
        let start = parser.take_token_if(TokenType::If)?.span.start;
        let condition = Box::new(parser.parse()?);

        let mut true_branch = Vec::new();
        while let Ok(expression) = parser.parse() {
            true_branch.push(expression);
        }

        let false_branch = if parser.take_token_if(TokenType::Else).is_ok() {
            let mut false_branch = Vec::new();
            while let Ok(expression) = parser.parse() {
                false_branch.push(expression);
            }
            Some(false_branch)
        } else {
            None
        };
        let end = parser.take_token_if(TokenType::End)?.span.end;

        Ok(Self {
            condition,
            then_branch: true_branch,
            else_branch: false_branch,
            span: start..end,
        })
    }

    fn span(&self) -> tokenizer::Span {
        self.span
    }
}

#[test]
fn test_if() {
    let source = r"if 1
    2
else
    3
end";
    assert_eq!(
        Parser::from(Tokenizer::from(source)).parse::<If>().unwrap(),
        If {
            condition: Box::new(Expression::Literal(Literal::Integer(1))),
            then_branch: vec![Statement::Expression(Expression::Literal(
                Literal::Integer(2)
            ))],
            else_branch: Some(vec![Statement::Expression(Expression::Literal(
                Literal::Integer(3)
            ))]),
        }
    );
}
