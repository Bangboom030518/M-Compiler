#[derive(Debug, PartialEq)]
pub struct BinaryExpression {
    pub operator: BinaryOperator,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

impl<'a> From<Pair<'a>> for BinaryExpression {
    fn from(pair: Pair<'a>) -> Self {
        let pairs = pair.into_inner();
        
        let primary = |pair: Pair|
            match pair.as_rule() {
                Rule::binary_term => BinaryExpression::from(pair),
                Rule::binary_expression => BinaryExpression::from(pair),
                rule => unreachable!("'{:?}' is invalid", rule)
            };

        let infix = |left: Expression, operator: Pair, right: Expression| {
            println!("I was called!");
            println!("{:?} {:?} {:?}", &left, &operator.as_rule(), &right);
            let operator = match operator.as_rule() {
                Rule::plus => BinaryOperator::Plus,
                Rule::minus => BinaryOperator::Minus,
                Rule::multiply => BinaryOperator::Multiply,
                Rule::divide => BinaryOperator::Divide,
                Rule::modulo => BinaryOperator::Modulo,
                rule => unreachable!("'{:?}' is not a valid binary operator", rule),
            };
            Expression::Binary(BinaryExpression {
                operator,
                left: Box::new(left),
                right: Box::new(right),
            })
        };
        
        let result = PREC_CLIMBER.climb(
            pairs,
            primary,
            infix,
        );

        dbg!(result);
        BinaryExpression {
            left: Box::new(Expression::Literal(Literal::Number(1.0))),
            right: Box::new(Expression::Literal(Literal::Number(1.0))),
            operator: BinaryOperator::Plus
        }
    }
}
