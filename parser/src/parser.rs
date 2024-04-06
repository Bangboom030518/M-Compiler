use crate::Parse;
use std::collections::HashSet;
use std::sync::Arc;
use tokenizer::{AsSpanned, Spanned, Token, TokenType, TokenTypeBitFields, Tokenizer};

#[derive(Clone, Copy, Debug, thiserror::Error)]
#[error("Parse error")]
pub(crate) struct Error {
    position: usize,
    kind: Kind,
    recoverable: bool,
}

impl Error {
    pub(crate) fn recoverable(self) -> Self {
        Self {
            recoverable: true,
            ..self
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum Kind {
    UnexpectedToken,
    UnexpectedIdentifier(&'static [&'static str]),
}

#[derive(Debug)]
pub struct Parser {
    tokenizer: Tokenizer,
    tokens: Vec<Spanned<Arc<Token>>>,
    position: usize,
    expected_tokens: TokenTypeBitFields,
}

impl From<Tokenizer> for Parser {
    fn from(tokenizer: Tokenizer) -> Self {
        Self {
            tokenizer,
            tokens: Vec::new(),
            expected_tokens: TokenTypeBitFields::default(),
            position: 0,
        }
    }
}

macro_rules! special {
    ($take_name:ident, $peek_name:ident, $variant_name:ident, $return_type:ident) => {
        pub(crate) fn $peek_name(&mut self) -> Result<tokenizer::Spanned<$return_type>, Error> {
            use tokenizer::AsSpanned;

            let token = self.peek_token_if(TokenType::$variant_name)?;
            let Token::$variant_name(value) = token.value.as_ref() else {
                unreachable!()
            };

            // TODO: clone
            Ok(value.clone().spanned(token.span))
        }

        pub(crate) fn $take_name(&mut self) -> Result<tokenizer::Spanned<$return_type>, Error> {
            let token = self.$peek_name()?;
            self.position += 1;
            self.expected_tokens.clear();
            Ok(token)
        }
    };
}

impl Parser {
    pub(crate) fn parse_error(&self, error: Error) -> crate::ParseError {
        match error.kind {
            Kind::UnexpectedToken => crate::ParseError::UnexpectedToken {
                expected: self.expected_tokens,
                found: self
                    .tokens
                    .get(error.position)
                    .expect("invalid error position")
                    .as_ref()
                    .map(|token| token.as_ref().clone()),
            },
            Kind::UnexpectedIdentifier(expected) => {
                let ident = self
                    .tokens
                    .get(error.position)
                    .expect("invalid error position")
                    .as_ref()
                    .map(|token| token.as_ref().clone())
                    .map(|token| match token {
                        Token::Ident(ident) => ident,
                        _ => {
                            unreachable!("unexpected identifier errors only exist on ident tokens")
                        }
                    });

                crate::ParseError::UnexpectedIdentifier {
                    expected,
                    found: crate::Ident(ident.value).spanned(ident.span),
                }
            }
        }
    }

    pub(crate) const fn unexpected_ident(&self, expected: &'static [&'static str]) -> Error {
        Error {
            position: self.position,
            kind: Kind::UnexpectedIdentifier(expected),
            recoverable: false,
        }
    }

    fn peek_token(&mut self) -> Spanned<Arc<Token>> {
        self.tokens
            .get(self.position)
            .map(|spanned| spanned.as_ref().map(Arc::clone))
            .unwrap_or_else(|| {
                self.tokens.push(self.tokenizer.take().map(Arc::new));
                self.tokens.last().unwrap().as_ref().map(Arc::clone)
            })
    }

    pub(crate) fn parse<T>(&mut self) -> Result<Spanned<T>, Error>
    where
        T: Parse,
    {
        self.scope(T::parse)
    }

    // TODO: rename
    pub(crate) fn scope<T>(
        &mut self,
        f: impl FnOnce(&mut Self) -> Result<T, Error>,
    ) -> Result<T, Error> {
        let start = self.position;
        let result = f(self);
        if result.is_err() {
            self.position = start;
        }
        result
    }

    pub(crate) fn parse_csv<T>(&mut self) -> Vec<Spanned<T>>
    where
        T: Parse,
    {
        let mut values = Vec::new();
        while let Ok(value) = self.parse() {
            values.push(value);
            if self.take_token_if(TokenType::Comma).is_err() {
                break;
            }
        }

        values
    }

    pub(crate) fn peek_token_if(&mut self, kind: TokenType) -> Result<Spanned<Arc<Token>>, Error> {
        self.expected_tokens.insert(kind);
        let token = self.peek_token();
        if token.value.kind() == kind {
            Ok(token)
        } else {
            Err(Error {
                position: self.position,
                kind: Kind::UnexpectedToken,
                recoverable: false,
            })
        }
    }

    pub(crate) fn take_token_if(&mut self, token: TokenType) -> Result<Spanned<Arc<Token>>, Error> {
        let token = self.peek_token_if(token)?;
        self.position += 1;
        self.expected_tokens.clear();
        Ok(token)
    }

    pub(crate) const fn empty_span(&self) -> tokenizer::Span {
        self.position..self.position
    }

    special!(take_string, peek_string, String, String);
    special!(take_ident, peek_ident, Ident, String);
    special!(take_float, peek_float, Float, f64);
    special!(take_integer, peek_integer, Integer, u128);
    special!(take_char, peek_char, Char, char);
}
