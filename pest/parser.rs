enum Statement {
    Expression(Expression),
    Import(Import)
}


struct Import;

enum Literal {
    String(String),
    Integer(isize),
    Char(char),
    Float(f64)
}

enum UnaryOperator {
    LogicalNOT,
    Negate,
}

enum BinaryOperator {
    Plus,
    Minus,
    Multiply,
    Divide,
    Exponent,
    Modulo,
    LogicalOR,
    LogicalAND,
}

struct UnaryExpression {
    operator: UnaryOperator,
    operand: Box<Expression>,
}

struct BinaryExpression {
    operator: BinaryOperator,
    left: Box<Expression>,
    right: Box<Expression>
}

enum Expression {
    Literal(Literal),
    Binary(BinaryExpression),
    Unary(UnaryExpression)
}


fn parse() {

}