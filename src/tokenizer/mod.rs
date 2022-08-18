use lazy_static::lazy_static;
use regex::Regex;
use std::fs;

lazy_static! {
    static ref COMMENTS_PATTERN: Regex =
        Regex::new(r"//.*|/\*[\s\S]\*/").expect("Couldn't create regex 'COMMENTS_PATTERN'");
}

// TODO: add other keywords
#[derive(Debug)]
pub enum Keyword {
    Const,
    Import,
    From,
}

#[derive(Debug)]
pub enum Token {
    String(String),
    Integer(usize),
    Char(char),
    List(Vec<Token>),
    Keyword(Keyword),
    Decimal(f64),
    Identifier(String),
    OpenBracket,
    CloseBracket,
    Assignment,
    OpenBrace,
    CloseBrace,
    Semicolon
}

fn remove_comments(input: &str) -> String {
    COMMENTS_PATTERN.replace_all(input, "").to_string()
}

fn tokenize(input: &str) -> Vec<Token> {
    let input = remove_comments(input);
    let mut tokens: Vec<Token> = Vec::new();
    let mut chars = input.chars();
    while let Some(ch) = chars.next() {
        match ch {
            '(' => tokens.push(Token::OpenBracket),
            ')' => tokens.push(Token::CloseBracket),
            '=' => tokens.push(Token::Assignment),
            '{' => tokens.push(Token::OpenBrace),
            '}' => tokens.push(Token::CloseBrace),
            ';' => tokens.push(Token::Semicolon),
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
            '\'' => {
                let ch = chars
                    .next()
                    .expect("Unexpected end of file: \"'\" should be followed by a character");

                tokens.push(Token::Char(ch));

                let ch = chars
                    .next()
                    .expect("Unexpected end of file: expected \"'\"");
                if ch != '\'' {
                    panic!("Chars can only contain 1 character");
                }
            }
            _ if ch.is_alphabetic() => {
                let mut contents = String::from(ch);
                while let Some(ch) = chars.next() {
                    if ch.is_alphanumeric() || ch == '_' {
                        contents.push(ch);
                    } else {
                        break;
                    }
                }
                let token = match contents.as_str() {
                    "const" => Token::Keyword(Keyword::Const),
                    "import" => Token::Keyword(Keyword::Import),
                    "from" => Token::Keyword(Keyword::From),
                    _ => Token::Identifier(contents),
                };
                tokens.push(token);
            }

            _ if ch.is_numeric() => {
                let mut contents = String::from(ch);
                let mut decimal = false;
                while let Some(ch) = chars.next() {
                    if ch == '.' {
                        decimal = true;
                        contents.push(ch);
                    } else if ch.is_numeric() {
                        contents.push(ch);
                    } else {
                        break;
                    }
                }
                tokens.push(if decimal {
                    Token::Decimal(
                        contents
                            .parse::<f64>()
                            .unwrap_or_else(|_| panic!("Couldn't convert {} to f64", contents)),
                    )
                } else {
                    Token::Integer(
                        contents
                            .parse::<usize>()
                            .unwrap_or_else(|_| panic!("Couldn't convert {} to usize", contents)),
                    )
                })
            }
            _ if ch.is_whitespace() => {}

            _ => {
                println!("Couldn't tokenize '{}'", ch)
            }
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
