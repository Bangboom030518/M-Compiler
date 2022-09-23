use peg::parser;

#[derive(Debug)]
pub enum Literal {
    Number(String),
    List(Vec<Expression>),
    Char(char),
}
#[derive(Debug)]
pub enum Expression {
    Literal(Literal),
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

        pub rule list() -> Vec<Expression>
          = "[" list:(expression() ** (_ "," _)) "]" { list }
    }
}

