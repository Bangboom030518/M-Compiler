use crate::parser::Parser;
use crate::{Error, Expression, Parse, Statement};
use tokenizer::{AsSpanned, Spanned, TokenType};

#[derive(Debug, Clone, PartialEq)]
pub struct If {
    pub condition: Box<Spanned<Expression>>,
    pub then_branch: Vec<Spanned<Statement>>,
    pub else_branch: Option<Vec<Spanned<Statement>>>,
}

impl Parse for If {
    fn parse(parser: &mut Parser) -> Result<Spanned<Self>, Error> {
        let start = parser.take_token_if(TokenType::If)?.start();
        let condition = Box::new(parser.parse()?);
        parser.take_token_if(TokenType::Then)?;
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
        let end = parser.take_token_if(TokenType::End)?.end();

        Ok(Self {
            condition,
            then_branch: true_branch,
            else_branch: false_branch,
        }
        .spanned(start..end))
    }
}

#[cfg(ignore)]
#[test]
fn test_if() {
    use tokenizer::despan_vec;

    let source = r"if 1
    2
else
    3
end";
    let expr = Parser::from(Tokenizer::from(source))
        .parse::<If>()
        .unwrap()
        .value;
    assert_eq!(
        expr.condition.value,
        Expression::Literal(Literal::Integer(1))
    );
    assert_eq!(
        despan_vec(expr.then_branch),
        vec![Statement::Expression(Expression::Literal(
            Literal::Integer(2)
        ))]
    );
    assert_eq!(
        despan_vec(expr.else_branch.unwrap()),
        vec![Statement::Expression(Expression::Literal(
            Literal::Integer(3)
        ))]
    );
}
