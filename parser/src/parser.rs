use crate::internal::prelude::*;
use crate::Error;
use std::collections::HashSet;
use tokenizer::{SpannedToken, TokenType};

#[derive(Debug)]
pub struct Parser {
    tokenizer: Tokenizer,
    tokens: Vec<SpannedToken>,
    position: usize,
    scope_cache: scope::Cache,
    expected_tokens: HashSet<TokenType>,
    pub(crate) scope: scope::Id,
}

impl From<Tokenizer> for Parser {
    fn from(tokenizer: Tokenizer) -> Self {
        Self {
            tokenizer,
            tokens: Vec::new(),
            expected_tokens: HashSet::new(),
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

impl Parser {
    pub fn create_scope(&mut self) -> scope::Id {
        let id = self.scope_cache.create_scope(self.scope);
        self.scope = id;
        id
    }

    pub fn get_scope(&mut self, id: scope::Id) -> &mut Scope {
        &mut self.scope_cache[id]
    }

    // TODO: scope guard
    pub fn exit_scope(&mut self) {
        self.scope = self.scope_cache[self.scope].parent.unwrap();
    }

    fn peek_token(&mut self) -> &SpannedToken {
        let token = self.tokens.get(self.position).unwrap_or_else(|| {
            self.tokens.push(self.tokenizer.take());
            self.tokens.last().unwrap()
        });

        if matches!(token.token, Token::Comment(_)) {
            self.position += 1;
            self.peek_token()
        } else {
            token
        }
    }

    fn take_token(&mut self) -> &SpannedToken {
        let token = self.peek_token();
        self.position += 1;
        self.expected_tokens.clear();
        token
    }

    pub fn parse<T>(&mut self) -> Result<T, Error>
    where
        T: Parse,
    {
        self.scope(T::parse)
    }

    // TODO: rename
    pub fn scope<T>(&mut self, f: impl FnOnce(&mut Self) -> Result<T, Error>) -> Result<T, Error> {
        let start = self.position;
        let result = f(self);
        if result.is_err() {
            self.position = start;
        }
        result
    }

    pub fn parse_csv<T>(&mut self) -> Vec<T>
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

    pub fn peek_token_if<'b>(&mut self, kind: TokenType) -> Result<&SpannedToken, Error> {
        let token = self.peek_token();
        self.expected_tokens.insert(token.kind());
        if token.kind() == kind {
            Ok(token)
        } else {
            Err(Error {
                expected: &self.expected_tokens,
                found: &token,
            })
        }
    }

    #[must_use]
    pub fn take_token_if<'b>(&mut self, token: TokenType) -> Result<&SpannedToken, Error> {
        let token = self.peek_token_if(token)?;
        self.position += 1;
        self.expected_tokens.clear();
        Ok(token)
    }
}
