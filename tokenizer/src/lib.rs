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
                    Self::Integer(_) => TokenType::Integer,
                    Self::Float(_) => TokenType::Float,
                }
            }

            fn spanned(self, span: Span) -> Spanned<Self> {
                Spanned {
                    value: self,
                    span,
                }
            }
        }

        #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, bitfields::BitFields)]
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn with<U>(&self, value: U) -> Spanned<U> {
        Spanned {
            value,
            span: self.span.clone(),
        }
    }

    pub fn map<U>(self, mapping: impl FnOnce(T) -> U) -> Spanned<U> {
        Spanned {
            value: mapping(self.value),
            span: self.span,
        }
    }

    pub fn as_ref(&self) -> Spanned<&T> {
        Spanned {
            value: &self.value,
            span: self.span.clone(),
        }
    }

    pub fn start(&self) -> usize {
        self.span.start
    }

    pub fn end(&self) -> usize {
        self.span.end
    }
}

impl Spanned<Token> {
    pub fn kind(&self) -> TokenType {
        self.value.kind()
    }
}

#[test]
fn test_bit_fields() {
    let mut fields = TokenTypeBitFields::default();
    fields.insert(TokenType::Struct);
    let set = fields.hash_set().into_iter().collect_vec();
    assert_eq!(set, vec![TokenType::Struct]);
}

pub trait SpannedResultExt<T, E> {
    fn with_spanned<U>(self, value: U) -> Result<Spanned<U>, E>;
    fn map_spanned<U>(self, mapping: impl FnOnce(T) -> U) -> Result<Spanned<U>, E>;
    fn and_then_spanned<U>(self, mapping: impl FnOnce(T) -> Result<U, E>) -> Result<Spanned<U>, E>;
}

impl<T, E> SpannedResultExt<T, E> for Result<Spanned<T>, E> {
    fn with_spanned<U>(self, value: U) -> Result<Spanned<U>, E> {
        self.map(|token| token.with(value))
    }

    fn map_spanned<U>(self, mapping: impl FnOnce(T) -> U) -> Result<Spanned<U>, E> {
        self.map(|spanned| spanned.map(mapping))
    }

    fn and_then_spanned<U>(self, mapping: impl FnOnce(T) -> Result<U, E>) -> Result<Spanned<U>, E> {
        self.and_then(|spanned| mapping(spanned.value).map(|value| value.spanned(spanned.span)))
    }
}

pub fn despan_vec<T>(list: Vec<Spanned<T>>) -> Vec<T> {
    list.into_iter().map(|item| item.value).collect()
}

pub trait AsSpanned {
    fn spanned(self, span: Span) -> Spanned<Self>
    where
        Self: Sized,
    {
        Spanned { value: self, span }
    }
}

impl<T> AsSpanned for T {}

impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", format!("{self:?}").to_lowercase())
    }
}

impl std::fmt::Display for TokenTypeBitFields {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let tokens = self
            .hash_set()
            .into_iter()
            .map(|token| token.to_string())
            .intersperse_with(|| ", ".to_string())
            .collect::<String>();
        write!(f, "{}", tokens)
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
                // TODO: span properly
                self.0.next();
                self.0.peeking_take_while(|&ch| ch != '\n').for_each(drop);
                self.take().value
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
                self.take().value
            }
            _ => Token::Divide,
        }
    }

    pub fn take(&mut self) -> Spanned<Token> {
        let start = self.0.index;
        let Some(ch) = self.0.next() else {
            return Token::Eoi.spanned(self.0.index..self.0.index);
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

        token.spanned(start..self.0.index)
    }

    // TODO: trait!
    #[cfg(test)]
    fn into_iter(mut self) -> impl Iterator<Item = Spanned<Token>> {
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
    let input = "+ / * /*Hello World*/ struct identifier_test";
    let tokenizer: Tokenizer = input.into();
    let tokens = tokenizer.into_iter().map(|token| token.value).collect_vec();
    assert_eq!(
        tokens,
        vec![
            Token::Plus,
            Token::Divide,
            Token::Multiply,
            Token::Struct,
            Token::Ident("identifier_test".to_string())
        ]
    );
}

#[test]
fn tokenize_string() {
    let input = r#"+ "Hello World \r \u{001B}""#;
    let tokenizer: Tokenizer = input.into();
    let tokens = tokenizer.into_iter().map(|token| token.value).collect_vec();
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
    let tokens = tokenizer.into_iter().map(|token| token.value).collect_vec();
    assert_eq!(
        tokens,
        vec![Token::Integer(0xf), Token::Integer(1), Token::Float(0.5),]
    );
}
