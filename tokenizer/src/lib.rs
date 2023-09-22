#![feature(iter_collect_into)]
use itertools::Itertools;
use std::iter::Peekable;
use std::str::Chars;

#[derive(Clone, Debug, Default, PartialEq)]
pub enum Token {
    Const,
    Function,
    Type,
    // TODO: ?
    // Enum,
    Exponent,
    Union,
    Struct,
    Assignment,
    Plus,
    Minus,
    Multiply,
    Divide,
    Indent,
    Dot,
    Equality,
    Bang,
    #[default]
    Illegal,
    String(String),
    Char(char),
    Ident(String),
    Comment(String),
    Integer(u64),
    Float(f64),
}

pub struct Tokenizer<'a>(Peekable<Chars<'a>>);

impl<'a> From<&'a str> for Tokenizer<'a> {
    fn from(value: &'a str) -> Self {
        Self(value.chars().peekable())
    }
}

impl<'a> Tokenizer<'a> {
    fn take_ident_or_keyword(&mut self, first_char: char) -> Token {
        let ident: String = std::iter::once(first_char)
            .chain(
                self.0
                    .peeking_take_while(|&ch| ch.is_alphanumeric() || ch == '_'),
            )
            .collect();

        match ident.as_str() {
            "const" => Token::Const,
            "struct" => Token::Struct,
            "union" => Token::Union,
            "function" => Token::Function,
            "type" => Token::Type,
            _ => Token::Ident(ident),
        }
    }

    fn take_escaped_char(&mut self) -> Option<char> {
        let ch = match self.0.next()? {
            'n' => '\n',
            't' => '\t',
            'r' => '\r',
            '0' => '\0',
            'u' => {
                if self.0.next()? != '{' {
                    return None;
                }
                let digits: String = self.0.peeking_take_while(|&ch| ch != '}').collect();
                self.0.next().unwrap();
                let code = u32::from_str_radix(&digits, 16).ok()?;
                char::from_u32(code)?
            }
            '"' => '"',
            '\'' => '\'',
            '\\' => '\\',
            _ => return None,
        };
        Some(ch)
    }

    fn take_string(&mut self) -> Token {
        let mut string = String::new();
        while let Some(ch) = self.0.next() {
            let ch = match ch {
                '\\' => match self.take_escaped_char() {
                    Some(ch) => ch,
                    None => return Token::Illegal,
                },
                '"' => break,
                ch => ch,
            };
            string.push(ch);
        }
        Token::String(string)
    }

    fn take_number_base(&mut self, base: u32) -> Token {
        self.0.next();
        let digits: String = self.0.peeking_take_while(|ch| ch.is_digit(base)).collect();
        u64::from_str_radix(&digits, base)
            .map(Token::Integer)
            .unwrap_or_default()
    }

    fn take_number(&mut self, first_char: char) -> Token {
        if first_char == '0' {
            match self.0.peek() {
                Some(&'x' | &'X') => return self.take_number_base(16),
                Some(&'o' | &'O') => return self.take_number_base(8),
                Some(&'b' | &'B') => return self.take_number_base(2),
                _ => {}
            }
        }

        let mut digits: String = std::iter::once(first_char)
            .chain(
                self.0
                    .peeking_take_while(|&ch| ch.is_ascii_digit() || ch == '_'),
            )
            .collect();

        if self.0.peek() != Some(&'.') {
            return digits.parse().map(Token::Integer).unwrap_or_default();
        }
        self.0.next().unwrap();
        digits.push('.');

        self.0
            .peeking_take_while(|&ch| ch.is_ascii_digit() || ch == '_')
            .collect_into(&mut digits);

        digits.parse().map(Token::Float).unwrap_or_default()
    }

    fn take_comment_or_divide(&mut self) -> Token {
        match self.0.peek() {
            Some(&'/') => {
                self.0.next();
                Token::Comment(self.0.peeking_take_while(|&ch| ch != '\n').collect())
            }
            Some(&'*') => {
                self.0.next();
                let mut comment = String::new();
                while let Some(ch) = self.0.next() {
                    if ch == '*' && self.0.peek() == Some(&'/') {
                        self.next();
                        break;
                    }
                    comment.push(ch)
                }
                Token::Comment(comment)
            }
            _ => Token::Divide,
        }
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let ch = self.0.next()?;
        let token = match ch {
            '+' => Token::Plus,
            '-' => Token::Minus,
            '.' => Token::Dot,
            '\t' => Token::Indent,
            '!' => Token::Bang,
            '"' => self.take_string(),
            '*' => {
                if self.0.peek() == Some(&'*') {
                    self.0.next();
                    Token::Exponent
                } else {
                    Token::Multiply
                }
            }
            '=' => {
                if self.0.peek() == Some(&'=') {
                    self.0.next();
                    Token::Equality
                } else {
                    Token::Assignment
                }
            }
            '/' => self.take_comment_or_divide(),
            ch if ch.is_whitespace() => self.next()?,
            ch if ch.is_alphabetic() || ch == '_' => self.take_ident_or_keyword(ch),
            ch if ch.is_numeric() => self.take_number(ch),
            _ => Token::Illegal,
        };
        Some(token)
    }
}

#[test]
fn tokenize_stuff() {
    let input = "+ / * /*Hello World*/ const identifier_test";
    let tokenizer: Tokenizer = input.into();
    let tokens = tokenizer.collect_vec();
    assert_eq!(
        tokens,
        vec![
            Token::Plus,
            Token::Divide,
            Token::Multiply,
            Token::Comment("Hello World".to_string()),
            Token::Const,
            Token::Ident("identifier_test".to_string())
        ]
    );
}

#[test]
fn tokenize_string() {
    let input = r#"+ "Hello World \r \u{001B}""#;
    let tokenizer: Tokenizer = input.into();
    let tokens = tokenizer.collect_vec();
    assert_eq!(
        tokens,
        vec![
            Token::Plus,
            Token::String("Hello World \r \u{001B}".to_string())
        ]
    );
}

#[test]
fn tokenize_number() {
    let input = "0xf 1 0.5";
    let tokenizer: Tokenizer = input.into();
    let tokens = tokenizer.collect_vec();
    assert_eq!(
        tokens,
        vec![Token::Integer(0xf), Token::Integer(1), Token::Float(0.5),]
    );
}
