use peg::parser;

const BINARY_DIGITS: &[char] = &['0', '1'];
const DENARY_DIGITS: &[char] = &['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];

#[derive(Debug)]
pub enum Literal {
    Number(String),
    List(Vec<Expression>),
    Char(char),
}

#[derive(Debug)]
pub enum BinaryOperator {
    Multiply
}

#[derive(Debug)]
pub struct BinaryExpression {
    left: Expression,
    right: Expression,
    operator: BinaryOperator,
}

#[derive(Debug)]
pub enum Expression {
    Literal(Literal),
}

#[allow(clippy::cast_possible_truncation)]
fn digits_from_slice(digits: &[char], base_digits: &[char]) -> Vec<u8> {
    digits
        .iter()
        .map(|&digit| {
            base_digits
                .iter()
                .position(|&character| character == digit)
                .unwrap_or_else(|err| panic!("Digit in base {} should not be {}", base_digits.len(), digit)) as u8
        })
        .collect()
}

parser! {
    grammar m_parser() for str {
        rule whitespace() = [' ' | '\n' | '\t']

        // Comments like this one
        rule line_comment() = "//" (!"\n" [_])* ("\n" / ![_])

        /* Comments like this */
        rule inline_comment() = "/*" (!"*/" [_])* "*/"

        rule _() = quiet!{ (whitespace() / "\n" / inline_comment() / line_comment())* }

        rule expression() -> Expression
          = value:(literal()) {
            Expression::Literal(value)
          }

        rule literal() -> Literal
          = value:(number()) {
            Literal::Number(value)
          } / value:(char()) {
            Literal::Char(value)
          }

        rule char() -> char
          = character:['a'] {
            character
          }

        rule number() -> String
          = number:$(['0'..='9']+) {
            String::from(number)
        }

        /// Matches number literals
        rule number() -> Number
          = negation_sign:"-"? "0b" whole_digits:(['0'..='1']*) "." fractional_digits:(['0'..='1']*) {
            // Binary floats
            Number {
                whole_digits: digits_from_slice(&whole_digits, BINARY_DIGITS),
                fractional_digits: digits_from_slice(&fractional_digits, BINARY_DIGITS),
                base: Base::Binary,
                positive: negation_sign.is_none()
            }
        } / negation_sign:"-"? "0b" whole_digits:(['0'..='1']+) {
            // Binary ints
            Number {
                whole_digits: digits_from_slice(&whole_digits, BINARY_DIGITS),
                fractional_digits: Vec::new(),
                base: Base::Binary,
                positive: negation_sign.is_none()
            }
        } / negation_sign:"-"? whole_digits:(['0'..='9']*) "." fractional_digits:(['0'..='9']*) {
            // Denary floats
            Number {
                whole_digits: digits_from_slice(&whole_digits, DENARY_DIGITS),
                fractional_digits: digits_from_slice(&fractional_digits, DENARY_DIGITS),
                base: Base::Denary,
                positive: negation_sign.is_none()
            }
        } / negation_sign:"-"? whole_digits:(['0'..='9']+) {
            // Denary ints
            Number {
                whole_digits: digits_from_slice(&whole_digits, DENARY_DIGITS),
                fractional_digits: Vec::new(),
                base: Base::Denary,
                positive: negation_sign.is_none()
            }
        }

        /// Matches literals
        rule literal() -> Literal
          = value:number() {
            Literal::Number(value)
        }

        pub rule expression() -> Expression
         = precedence!{
            lhs:(@) _ "+" _ rhs:@ {
                Expression::Add(Box::new(lhs), Box::new(rhs))
            }
            lhs:(@) _ "-" _ rhs:@ {
                Expression::Sub(Box::new(lhs), Box::new(rhs))
            }
            --
            lhs:(@) _ "*" _ rhs:@ {
                Expression::Mul(Box::new(lhs), Box::new(rhs))
            }
            lhs:(@) _ "/" _ rhs:@ {
                Expression::Div(Box::new(lhs), Box::new(rhs))
            }
            --
            lhs:@ _ "^" _ rhs:(@) {
                Expression::Pow(Box::new(lhs), Box::new(rhs))
            }
            --
            "-" _ expression:(@) {
                Expression::Negate(Box::new(expression))
            }
            --
            _ value:literal() _ {
                Expression::Literal(value)
            }
            _ "(" _ expression:expression() _ ")" _ { expression }
        }

        pub rule list() -> Vec<Expression>
          = "[" list:(expression() ** (_ "," _)) "]" { list }
    }
}
