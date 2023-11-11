use crate::internal::prelude::*;

#[derive(Debug)]
pub struct Parser<'a> {
    tokenizer: Tokenizer<'a>,
    tokens: Vec<Token>,
    position: usize,
    indent: u8,
    scope_cache: scope::Cache,
    pub(crate) scope: scope::Id,
}

impl<'a> From<Tokenizer<'a>> for Parser<'a> {
    fn from(tokenizer: Tokenizer<'a>) -> Self {
        Self {
            tokenizer,
            tokens: Vec::new(),
            position: 0,
            indent: 0,
            scope_cache: scope::Cache::new(),
            scope: scope::Cache::ROOT_SCOPE,
        }
    }
}

impl<'a> From<Parser<'a>> for scope::File {
    fn from(value: Parser<'a>) -> Self {
        Self {
            root: value.scope,
            cache: value.scope_cache,
        }
    }
}

impl<'a> Parser<'a> {
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

    pub fn peek_any(&mut self) -> Option<Token> {
        let token = match self.tokens.get(self.position).cloned() {
            token @ Some(_) => token,
            None => {
                self.tokens.push(self.tokenizer.next()?);
                self.tokens.last().cloned()
            }
        }?;
        if matches!(token, Token::Comment(_)) {
            self.position += 1;
            self.peek_any()
        } else {
            Some(token)
        }
    }

    pub fn peek_token(&mut self) -> Option<Token> {
        let token = self.peek_any()?;
        if matches!(token, Token::Indent | Token::Newline) {
            self.position += 1;
            self.peek_token()
        } else {
            Some(token)
        }
    }

    pub fn take_token(&mut self) -> Option<Token> {
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
        let start = self.position;
        match T::parse(self) {
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
        for _ in 0..self.indent {
            if self.peek_any() == Some(Token::Indent) {
                self.position += 1;
            } else {
                self.position = start;
                return None;
            }
        }

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
        self.peek_any().is_none().then_some(())
    }

    pub fn peek_token_if<'b>(&mut self, token: &'b Token) -> Option<&'b Token> {
        if self.peek_token().as_ref() == Some(token) {
            Some(token)
        } else {
            None
        }
    }

    pub fn take_token_if<'b>(&mut self, token: &'b Token) -> Option<&'b Token> {
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

    pub fn indent(&mut self) {
        self.indent += 1;
    }

    pub fn unindent(&mut self) {
        self.indent -= 1;
    }
}
