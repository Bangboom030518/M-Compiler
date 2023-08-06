use super::super::binary::fmt_expression;
use crate::prelude::*;
use crate::Expression;

#[derive(Debug, Clone, PartialEq, Eq, Rand)]
pub struct If {
    condition: Box<Expression>,
    true_branch: Box<Expression>,
    false_branch: Option<Box<Expression>>,
}

impl If {
    fn condition(input: &str) -> IResult<Expression> {
        preceded(
            pair(tag("if"), whitespace),
            whitespace_delimited(Expression::parse),
        )(input)
    }

    fn true_branch(input: &str) -> IResult<Expression> {
        preceded(pair(tag("do"), whitespace), Expression::parse)(input)
    }

    fn false_branch(input: &str) -> IResult<Option<Expression>> {
        opt(preceded(
            whitespace_delimited(tag("else")),
            Expression::parse,
        ))(input)
    }
}

impl Parse for If {
    fn parse(input: &str) -> IResult<Self> {
        map(
            tuple((Self::condition, Self::true_branch, Self::false_branch)),
            Self::from,
        )(input)
    }
}

impl From<(Expression, Expression, Option<Expression>)> for If {
    fn from(
        (condition, true_branch, false_branch): (Expression, Expression, Option<Expression>),
    ) -> Self {
        Self {
            condition: Box::new(condition),
            true_branch: Box::new(true_branch),
            false_branch: false_branch.map(Box::new),
        }
    }
}

impl std::fmt::Display for If {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let Self {
            condition,
            true_branch,
            false_branch,
        } = self;
        write!(f, "if {condition} do ")?;
        if let Some(false_branch) = false_branch {
            fmt_expression(f, &true_branch.clone())?;
            write!(f, " else {false_branch}")
        } else {
            write!(f, "{true_branch}")
        }
    }
}

#[test]
fn if_manual_test() {
    let test_subject = If::from((
        Expression::Continue,
        Expression::If(If::from((Expression::Continue, Expression::Continue, None))),
        Some(Expression::Continue),
    ));
    let test_str = test_subject.to_string();
    assert_eq!(If::parse(&test_str), Ok(("", test_subject)));
}
