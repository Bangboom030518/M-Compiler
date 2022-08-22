use lazy_static::lazy_static;
use regex::Regex;
use std::str::Chars;

lazy_static! {
    static ref COMMENTS_PATTERN: Regex =
        Regex::new(r"//.*|/\*[\s\S]\*/").expect("Couldn't create regex 'COMMENTS_PATTERN'");
}

// TODO: add other keywords
#[derive(Debug, Clone)]
pub enum Keyword {
    Const,
    Import,
    From,
}

#[derive(Debug, Clone)]
pub enum Token {
    String(String),
    Integer(usize),
    Char(char),
    Keyword(Keyword),
    Decimal(f64),
    Identifier(String),
    OpenBracket,
    CloseBracket,
    Assignment,
    Plus,
    Minus,
    Asterisk,
    Slash,
    OpenBrace,
    CloseBrace,
    Semicolon,
}

fn remove_comments(input: &str) -> String {
    COMMENTS_PATTERN.replace_all(input, "").to_string()
}

fn operator_reader(current: char, _: &mut Chars) -> Option<Token> {
    match current {
        '(' => Some(Token::OpenBracket),
        ')' => Some(Token::CloseBracket),
        '=' => Some(Token::Assignment),
        '{' => Some(Token::OpenBrace),
        '}' => Some(Token::CloseBrace),
        ';' => Some(Token::Semicolon),
        '+' => Some(Token::Plus),
        '-' => Some(Token::Minus),
        '*' => Some(Token::Asterisk),
        '/' => Some(Token::Slash),
        _ => None
    }
}

fn string_reader(current: char, chars: &mut Chars) -> Option<Token> {
    if current != '"' {
        return None;
    };
    let mut contents = String::new();

    while let Some(ch) = chars.next() {
        if ch == '"' {
            break;
        } else {
            contents.push(ch);
        }
    }
    Some(Token::String(contents))
}

fn char_reader(current: char, chars: &mut Chars) -> Option<Token> {
    if current != '\'' {
        return None;
    };
    let ch = chars
        .next()
        .expect("Unexpected end of file: \"'\" should be followed by a character");

    let next = chars
        .next()
        .expect("Unexpected end of file: expected \"'\"");

    if next != '\'' {
        panic!("Chars can only contain 1 character");
    }
    Some(Token::Char(ch))
}

fn word_reader(current: char, chars: &mut Chars) -> Option<Token> {
    if !current.is_alphabetic() {
        return None;
    };
    let mut contents = String::from(current);
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
    Some(token)
}

fn number_reader(current: char, chars: &mut Chars) -> Option<Token> {
    if !current.is_numeric() {
        return None;
    };
    let mut contents = String::from(current);
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
    Some(if decimal {
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

fn backup_reader(current: char, _: &mut Chars) -> Option<Token> {
    if !current.is_whitespace() {
        println!("Couldn't tokenize '{}'", current);
    };
    None
}

pub fn tokenize(input: &str) -> Vec<Token> {
    let input = remove_comments(input);
    let readers: Vec<fn(char, &mut Chars) -> Option<Token>> = vec![
        operator_reader,
        string_reader,
        char_reader,
        word_reader,
        number_reader,
        backup_reader,
    ];
    let mut tokens: Vec<Token> = Vec::new();
    let mut chars = input.chars();
    while let Some(ch) = chars.next() {
        for reader in readers.clone() {
            if let Some(token) = reader(ch, &mut chars) {
                tokens.push(token);
                break;
            }
        }
    }
    tokens
}

