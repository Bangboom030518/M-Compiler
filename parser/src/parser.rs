use tokenizer::{SpannedToken, TokenType};

use crate::internal::prelude::*;

#[derive(Debug)]
pub struct Parser {
    tokenizer: Tokenizer,
    tokens: Vec<SpannedToken>,
    position: usize,
    scope_cache: scope::Cache,
    pub(crate) scope: scope::Id,
}

impl From<Tokenizer> for Parser {
    fn from(tokenizer: Tokenizer) -> Self {
        Self {
            tokenizer,
            tokens: Vec::new(),
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

    pub fn peek_any(&mut self) -> Option<SpannedToken> {
        let token = match self.tokens.get(self.position).cloned() {
            token @ Some(_) => token,
            None => {
                self.tokens.push(self.tokenizer.next()?);
                self.tokens.last().cloned()
            }
        }?;
        if matches!(token.token, Token::Comment(_)) {
            self.position += 1;

            self.peek_any()
        } else {
            Some(token)
        }
    }

    pub fn peek_token(&mut self) -> Option<SpannedToken> {
        let token = self.peek_any()?;
        if matches!(token, Token::Newline) {
            self.position += 1;
            self.peek_token()
        } else {
            Some(token)
        }
    }

    pub fn take_token(&mut self) -> Option<SpannedToken> {
        let token @ Some(_) = self.peek_token() else {
            return None;
        };
        self.position += 1;
        token
    }

    pub fn parse<T>(&mut self) -> Option<T>
    where
        T: Parse,
    {
        self.scope(T::parse)
    }

    // TODO: rename
    pub fn scope<T>(&mut self, f: impl FnOnce(&mut Self) -> Option<T>) -> Option<T> {
        let start = self.position;
        match f(self) {
            None => {
                self.position = start;
                None
            }
            result @ Some(_) => result,
        }
    }

    pub fn parse_csv<T>(&mut self) -> Vec<T>
    where
        T: Parse,
    {
        let mut values = Vec::new();
        while let Some(value) = self.parse() {
            values.push(value);
            if self.take_token_if(&Token::Comma).is_none() {
                break;
            }
        }

        values
    }

    pub fn parse_line<T>(&mut self) -> Option<T>
    where
        T: Parse,
    {
        // TODO: below
        /*
           function add = (Self self, Self other) Self
               // if @icmp(self, 2) & @icmp(other, 2)
               //     return 5
               a
               // a
        */
        let start = self.position;

        let Some(value) = self.parse() else {
            self.position = start;
            return None;
        };

        if self.take_newline_or_eof().is_some() {
            Some(value)
        } else {
            self.position = start;
            None
        }
    }

    pub fn take_newline_or_eof(&mut self) -> Option<()> {
        let value = self.peek_newline_or_eof();
        if value.is_some() {
            self.position += 1;
        }
        value
    }

    pub fn peek_newline_or_eof(&mut self) -> Option<()> {
        matches!(self.peek_any(), Some(Token::Newline) | None).then_some(())
    }

    pub fn peek_eof(&mut self) -> Option<()> {
        self.peek_token().is_none().then_some(())
    }

    pub fn peek_token_if<'b>(&mut self, token: &'b TokenType) -> Option<SpannedToken> {
        if self.peek_token().as_ref().kind() == Some(token) {
            Some(token)
        } else {
            None
        }
    }

    #[must_use]
    pub fn take_token_if<'b>(&mut self, token: &'b TokenType) -> Option<&'b SpannedToken> {
        let token = self.peek_token_if(token);
        if token.is_some() {
            self.position += 1;
        }
        token
    }

    pub fn take_newline(&mut self) -> Option<()> {
        let token = self.peek_newline();
        if token.is_some() {
            self.position += 1;
        }
        token
    }

    pub fn peek_newline(&mut self) -> Option<()> {
        matches!(self.peek_any(), Some(Token::Newline)).then_some(())
    }
}
