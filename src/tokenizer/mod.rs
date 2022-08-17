pub enum Bracket {
    Open,
    Close,
}

pub enum Token {
    String(&'static str),
    Integer(usize),
    List(Vec<Token>),
    OpenBracket,
    CloseBracket,
    Brace(Bracket),
    SquareBracket(Bracket),
    Unknown,
}

fn tokenize(input: &str) -> Vec<Token> {
    // input.chars().map(|value| -> Token {
    //     match value {
    //         '(' => Token::OpenBracket,
    //         // ')' => Token,
    //         '"' => {
                
    //         }
    //         _ => Token::Unknown
    //     }
       
    // }).collect()
    let tokens: Vec<Token> = Vec::new();
    // input.chars()
    // for ch in input.chars() {
    //     match ch {
    //         '(' => tokens.push(Token::OpenBracket),
    //         '"' => tokens.push(Token::String(""))
    //     }
    // };
    let chars = input.chars();
    while let Some(ch) = chars.next() {
        match ch {
            '(' => tokens.push(Token::OpenBracket),
            '"' => {
                
            }
            ' ' => {}
        }
    };
    tokens
}

fn main() {
    tokenize("")
}
