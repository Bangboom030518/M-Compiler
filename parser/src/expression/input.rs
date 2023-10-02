fn main() {
    Expression {
        left: Binary(Expression {
            left: Literal(Integer(1)),
            right: Literal(Integer(1)),
            operator: Multiply,
        }),
        right: Literal(Integer(1)),
        operator: Divide,
    };
    Expression {
        left: Literal(Integer(1)),
        right: Binary(Expression {
            left: Literal(Integer(1)),
            right: Literal(Integer(1)),
            operator: Divide,
        }),
        operator: Multiply,
    }
}
