use std::fs;

// TODO: add other keywords
#[derive(Debug)]
#[allow(dead_code)]
pub enum Keyword {
    Const,
}

#[derive(Debug)]
#[allow(dead_code)]
pub enum Token {
    String(String),
    Integer(usize),
    List(Vec<Token>),
    Keyword(Keyword),
    Identifier(String),
    OpenBracket,
    CloseBracket,
    OpenBrace,
    CloseBrace,
    Unknown,
}

fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut chars = input.chars();
    while let Some(ch) = chars.next() {
        match ch {
            '(' => tokens.push(Token::OpenBracket),
            ')' => tokens.push(Token::CloseBracket),
            '"' => {
                let mut contents = String::new();
                while let Some(ch) = chars.next() {
                    if ch == '"' {
                        break;
                    } else {
                        contents.push(ch);
                    }
                }
                tokens.push(Token::String(contents));
            }
            _ if ch.is_alphabetic() => {
                let mut contents = String::new();
                while let Some(ch) = chars.next() {
                    if ch.is_alphanumeric() || ch == '_' {
                        contents.push(ch);
                    } else {
                        break;
                    }
                }
                let token = match contents.as_str() {
                    "const" => Token::Keyword(Keyword::Const),
                    _ => Token::Identifier(contents),
                };
                tokens.push(token);
            }

            _ if ch.is_whitespace() => {}

            _ => {}
        }
    }
    tokens
}

pub fn main() {
    let input = fs::read_to_string("input.txt").expect("Failed to read file 'input.txt'");
    for token in tokenize(&input) {
        println!("{:?}", token)
    }
}
