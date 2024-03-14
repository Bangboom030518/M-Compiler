use crate::scope::{self, Scope};
use crate::Parse;
use std::sync::Arc;
use tokenizer::{Spanned, Token, TokenType, TokenTypeBitFields, Tokenizer};

#[derive(Clone, Copy, Debug, thiserror::Error)]
#[error("Parse error")]
pub(crate) struct Error {
    position: usize,
}

#[derive(Debug)]
pub struct Parser {
    tokenizer: Tokenizer,
    tokens: Vec<Spanned<Arc<Token>>>,
    position: usize,
    scope_cache: scope::Cache,
    expected_tokens: TokenTypeBitFields,
    pub(crate) scope: scope::Id,
}

impl From<Tokenizer> for Parser {
    fn from(tokenizer: Tokenizer) -> Self {
        Self {
            tokenizer,
            tokens: Vec::new(),
            expected_tokens: TokenTypeBitFields::default(),
            position: 0,
            scope_cache: scope::Cache::new(),
            scope: scope::Cache::ROOT_SCOPE,
        }
    }
}

impl From<Parser> for scope::File {
    fn from(value: Parser) -> Self {
        Self {
            root: value.scope,
            cache: value.scope_cache,
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
        crate::ParseError {
            expected: self.expected_tokens,
            found: self
                .tokens
                .get(error.position)
                .expect("invalid error position")
                .as_ref()
                .map(|token| token.as_ref().clone()),
        }
    }

    pub(crate) fn create_scope(&mut self) -> scope::Id {
        let id = self.scope_cache.create_scope(self.scope);
        self.scope = id;
        id
    }

    pub(crate) fn get_scope(&mut self, id: scope::Id) -> &mut Scope {
        &mut self.scope_cache[id]
    }

    // TODO: scope guard
    pub(crate) fn exit_scope(&mut self) {
        self.scope = self.scope_cache[self.scope].parent.unwrap();
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
            })
        }
    }

    pub(crate) fn take_token_if(&mut self, token: TokenType) -> Result<Spanned<Arc<Token>>, Error> {
        let token = self.peek_token_if(token)?;
        self.position += 1;
        self.expected_tokens.clear();
        Ok(token)
    }

    special!(take_string, peek_string, String, String);
    special!(take_ident, peek_ident, Ident, String);
    special!(take_float, peek_float, Float, f64);
    special!(take_integer, peek_integer, Integer, u128);
    special!(take_char, peek_char, Char, char);
}
