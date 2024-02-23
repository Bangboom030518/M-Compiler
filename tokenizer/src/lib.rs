#![feature(iter_collect_into)]

use itertools::Itertools;

macro_rules! define_token_enums {
    ($($variants:ident),*) => {
        #[derive(Clone, Debug, PartialEq)]
        pub enum Token {
            $($variants),*,
            String(String),
            Char(char),
            Ident(String),
            Comment(String),
            Integer(u128),
            Float(f64),
        }

        impl Default for Token {
            fn default() -> Self {
                Self::Illegal
            }
        }

        impl Token {
            pub fn kind(&self) -> TokenType {
                match self {
                    $(Self::$variants => TokenType::$variants,)*
                    Self::String(_) => TokenType::String,
                    Self::Char(_) => TokenType::Char,
                    Self::Ident(_) => TokenType::Ident,
                    Self::Comment(_) => TokenType::Comment,
                    Self::Integer(_) => TokenType::Integer,
                    Self::Float(_) => TokenType::Float,
                }
            }
        }

        #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
        pub enum TokenType {
            $($variants),*,
            String,
            Char,
            Ident,
            Comment,
            Integer,
            Float,
        }
    };
}

define_token_enums!(
    Const,
    Function,
    Type,
    If,
    Else,
    Let,
    End,
    Return,
    Exponent,
    Union,
    Struct,
    Assignment,
    Plus,
    Minus,
    Multiply,
    Divide,
    Dot,
    Remainder,
    Equal,
    At,
    NotEqual,
    GreaterThan,
    LessThan,
    GreaterThanOrEqual,
    LessThanOrEqual,
    Bang,
    OpenParen,
    CloseParen,
    Comma,
    Illegal,
    Eoi
);

pub type Span = std::ops::Range<usize>;

#[derive(Clone, Debug, Default, PartialEq)]
pub struct SpannedToken {
    pub token: Token,
    pub span: Span,
}

impl SpannedToken {
    pub fn kind(&self) -> TokenType {
        self.token.kind()
    }
}

#[derive(Debug)]
pub struct Tokenizer(TokenizerCharsIter);

#[derive(Debug)]
pub struct TokenizerCharsIter {
    source: Vec<char>,
    index: usize,
}

impl itertools::PeekingNext for TokenizerCharsIter {
    fn peeking_next<F>(&mut self, accept: F) -> Option<Self::Item>
    where
        F: FnOnce(&Self::Item) -> bool,
    {
        let ch = self.peek()?;

        if accept(&ch) {
            self.index += 1;
            Some(ch)
        } else {
            None
        }
    }
}

impl Iterator for TokenizerCharsIter {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        let ch = self.peek()?;
        self.index += 1;
        Some(ch)
    }
}

impl TokenizerCharsIter {
    fn peek(&self) -> Option<char> {
        self.source.get(self.index).copied()
    }
}

impl<'a> From<&'a str> for Tokenizer {
    fn from(source: &'a str) -> Self {
        Self(TokenizerCharsIter {
            source: source.chars().collect(),
            index: 0,
        })
    }
}

impl Tokenizer {
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
            "fn" => Token::Function,
            "type" => Token::Type,
            "if" => Token::If,
            "else" => Token::Else,
            "let" => Token::Let,
            "return" => Token::Return,
            "end" => Token::End,
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
        u128::from_str_radix(&digits, base)
            .map(Token::Integer)
            .unwrap_or_default()
    }

    fn take_number(&mut self, first_char: char) -> Token {
        if first_char == '0' {
            match self.0.peek() {
                Some('x' | 'X') => return self.take_number_base(16),
                Some('o' | 'O') => return self.take_number_base(8),
                Some('b' | 'B') => return self.take_number_base(2),
                _ => {}
            }
        }

        let mut digits: String = std::iter::once(first_char)
            .chain(
                self.0
                    .peeking_take_while(|&ch| ch.is_ascii_digit() || ch == '_'),
            )
            .collect();

        if self.0.peek() != Some('.') {
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
            Some('/') => {
                self.0.next();
                Token::Comment(self.0.peeking_take_while(|&ch| ch != '\n').collect())
            }
            Some('*') => {
                self.0.next();
                let mut comment = String::new();
                while let Some(ch) = self.0.next() {
                    if ch == '*' && self.0.peek() == Some('/') {
                        self.take();
                        break;
                    }
                    comment.push(ch)
                }
                Token::Comment(comment)
            }
            _ => Token::Divide,
        }
    }

    pub fn take(&mut self) -> SpannedToken {
        let start = self.0.index;
        let Some(ch) = self.0.next() else {
            return SpannedToken {
                token: Token::Eoi,
                span: self.0.index..self.0.index,
            };
        };
        let token = match ch {
            '+' => Token::Plus,
            '-' => Token::Minus,
            '%' => Token::Remainder,
            '.' => Token::Dot,
            ',' => Token::Comma,
            '@' => Token::At,
            '!' => {
                if self.0.peek() == Some('=') {
                    self.0.next();
                    Token::NotEqual
                } else {
                    Token::Bang
                }
            }
            '>' => {
                if self.0.peek() == Some('=') {
                    self.0.next();
                    Token::GreaterThanOrEqual
                } else {
                    Token::GreaterThan
                }
            }
            '<' => {
                if self.0.peek() == Some('=') {
                    self.0.next();
                    Token::LessThanOrEqual
                } else {
                    Token::LessThan
                }
            }
            '(' => Token::OpenParen,
            ')' => Token::CloseParen,
            '"' => self.take_string(),
            '*' => {
                if self.0.peek() == Some('*') {
                    self.0.next();
                    Token::Exponent
                } else {
                    Token::Multiply
                }
            }
            '=' => {
                if self.0.peek() == Some('=') {
                    self.0.next();
                    Token::Equal
                } else {
                    Token::Assignment
                }
            }
            '/' => self.take_comment_or_divide(),
            ch if ch.is_whitespace() => return self.take(),
            ch if ch.is_alphabetic() || ch == '_' => self.take_ident_or_keyword(ch),
            ch if ch.is_numeric() => self.take_number(ch),
            _ => Token::Illegal,
        };

        SpannedToken {
            token,
            span: start..self.0.index,
        }
    }

    // TODO: trait!
    #[cfg(test)]
    fn into_iter(mut self) -> impl Iterator<Item = SpannedToken> {
        std::iter::from_fn(move || {
            let token = self.take();
            if token.kind() == TokenType::Eoi {
                None
            } else {
                Some(token)
            }
        })
    }
}

#[test]
fn tokenize_stuff() {
    let input = "+ / * /*Hello World*/ const identifier_test";
    let tokenizer: Tokenizer = input.into();
    let tokens = tokenizer.into_iter().map(|token| token.token).collect_vec();
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
    let tokens = tokenizer.into_iter().map(|token| token.token).collect_vec();
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
    let tokens = tokenizer.into_iter().map(|token| token.token).collect_vec();
    assert_eq!(
        tokens,
        vec![Token::Integer(0xf), Token::Integer(1), Token::Float(0.5),]
    );
}
