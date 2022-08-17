pub enum Bracket {
    Open,
    Close,
}

pub enum Token {
    String(&'static str),
    Integer(usize),
    List(Vec<Token>),
    Bracket(Bracket),
    Brace(Bracket),
    SquareBracket(Bracket),
}

const INPUT: &'static str = "";


fn tokenize(input: &str) -> Vec<Token> {
    co
}
