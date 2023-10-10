use crate::internal::prelude::*;
use crate::Expression;

#[derive(Debug, Clone, PartialEq)]
pub struct If {
    pub condition: Box<Expression>,
    pub true_branch: Vec<Statement>,
    pub false_branch: Option<Vec<Statement>>,
}

impl Parse for If {
    fn parse(parser: &mut Parser) -> Option<Self> {
        parser.take_token_if(&Token::If)?;
        let condition = Box::new(parser.parse()?);
        parser.take_newline()?;
        parser.indent();

        let mut true_branch = Vec::new();
        while let Some(expression) = parser.parse_line() {
            true_branch.push(expression);
        }
        parser.unindent();

        let false_branch = if parser.take_token_if(&Token::Else).is_some() {
            parser.take_newline()?;
            parser.indent();
            let mut false_branch = Vec::new();
            while let Some(expression) = parser.parse_line() {
                false_branch.push(expression);
            }
            parser.unindent();
            Some(false_branch)
        } else {
            None
        };

        Some(Self {
            condition,
            true_branch,
            false_branch,
        })
    }
}

#[test]
fn test_if() {
    let source = r"if 1
    2
else
    3";
    assert_eq!(
        Parser::from(Tokenizer::from(source)).parse::<If>().unwrap(),
        If {
            condition: Box::new(Expression::Literal(Literal::Integer(1))),
            true_branch: vec![Statement::Expression(Expression::Literal(
                Literal::Integer(2)
            ))],
            false_branch: Some(vec![Statement::Expression(Expression::Literal(
                Literal::Integer(3)
            ))]),
        }
    );
}
