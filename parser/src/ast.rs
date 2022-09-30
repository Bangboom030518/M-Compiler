#[derive(Debug)]
pub enum Literal {
    Number(Number),
    List(Vec<Expression>),
    Char(char),
}

#[derive(Debug)]
pub enum Statement {
    Expression(Expression),
}

#[derive(Debug)]
pub enum Base {
    Binary = 2,
    Octal = 8,
    Denary = 10,
    Hexadecimal = 16,
}

impl Base {
    const BINARY_DIGITS: &[char] = &['0', '1'];
    const DENARY_DIGITS: &[char] = &['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
    const OCTAL_DIGITS: &[char] = &['0', '1', '2', '3', '4', '5', '6', '7'];
    const HEXADECIMAL_DIGITS: &[char] = &[
        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f',
    ];

    pub const fn get_digits(&self) -> &[char] {
        match self {
            Self::Binary => Self::BINARY_DIGITS,
            Self::Denary => Self::DENARY_DIGITS,
            Self::Octal => Self::OCTAL_DIGITS,
            Self::Hexadecimal => Self::HEXADECIMAL_DIGITS,
        }
    }

    fn as_number(&self) -> u8 {
        *self as u8
    }
}

impl std::fmt::Display for Base {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "{}", format!("{:?}", self).to_lowercase())
    }
}

impl TryFrom<u8> for Base {
    type Error = u8;

    fn try_from(number: u8) -> Result<Self, u8> {
        match number {
            2 => Ok(Self::Binary),
            8 => Ok(Self::Octal),
            10 => Ok(Self::Denary),
            16 => Ok(Self::Hexadecimal),
            number => Err(number),
        }
    }
}

#[derive(Debug)]
pub enum Number {
    Integer(Integer),
    Fractional(Fractional),
}

#[derive(Debug)]
pub struct Integer {
    pub digits: Vec<u8>,
    pub base: Base,
    pub sign: Sign,
}

/// Fractional
#[derive(Debug)]
pub struct Fractional {
    pub sign: Sign,
    pub whole_digits: Vec<u8>,
    pub fractional_digits: Vec<u8>,
    pub base: Base,
}

#[derive(Debug)]
pub enum Sign {
    Negative,
    Positive,
}

#[derive(Debug)]
pub enum BinaryOperator {
    Multiply,
    Add,
    Subtract,
    Divide,
    Exponent,
}

#[derive(Debug)]
pub struct BinaryExpression {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
    pub operator: BinaryOperator,
}

impl BinaryExpression {
    pub fn new(left: Expression, right: Expression, operator: BinaryOperator) -> Self {
        Self {
            left: Box::new(left),
            right: Box::new(right),
            operator,
        }
    }
}

#[derive(Debug)]
pub enum UnaryOperator {
    Negate,
    Bang,
}

#[derive(Debug)]
pub struct UnaryExpression {
    pub operand: Box<Expression>,
    pub operator: UnaryOperator,
}

#[derive(Debug)]
pub enum Expression {
    Literal(Literal),
    Binary(BinaryExpression),
    Unary(UnaryExpression),
}
