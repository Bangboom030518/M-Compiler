use itertools::Itertools;
use std::iter::Peekable;
use std::str::Chars;

#[derive(Clone, Debug, PartialEq, Eq)]
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
    Illegal,
    Ident(String),
    Comment(String),
    // Parenthesised(Vec<Token>),
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
