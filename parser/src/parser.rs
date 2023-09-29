use crate::internal::prelude::*;

#[derive(Debug)]
pub struct Parser<'a> {
    tokenizer: Tokenizer<'a>,
    tokens: Vec<Token>,
    position: usize,
    indent: u8,
}

impl<'a> From<Tokenizer<'a>> for Parser<'a> {
    fn from(tokenizer: Tokenizer<'a>) -> Self {
        Self {
            tokenizer,
            tokens: Vec::new(),
            position: 0,
            indent: 0,
        }
    }
}

impl<'a> Parser<'a> {
    pub fn peek_token(&mut self) -> Option<Token> {
        let token = self.tokens.get(self.position).cloned();
        let token = match token {
            token @ Some(_) => token,
            None => {
                self.tokens.push(self.tokenizer.next()?);
                self.tokens.last().cloned()
            }
        };
        token
    }

    // TODO: clone?
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

    pub fn parse_line<T>(&mut self) -> Option<T>
    where
        T: Parse,
    {
        let start = self.position;
        for _ in 0..self.indent {
            if !self.next_token_is(&Token::Indent) {
                self.position = start;
                return None;
            }
        }

        let Some(value) = self.parse() else {
            self.position = start;
            return None;
        };

        if self.next_newline_or_eof() {
            Some(value)
        } else {
            self.position = start;
            return None;
        }
    }

    pub fn next_token_is(&mut self, token: &Token) -> bool {
        if self.peek_token_is(token) {
            self.position += 1;
            true
        } else {
            false
        }
    }

    pub fn next_newline_or_eof(&mut self) -> bool {
        let value = self.peek_newline_or_eof();
        self.take_token();
        value
    }

    pub fn peek_newline_or_eof(&mut self) -> bool {
        matches!(self.peek_token(), Some(Token::Newline) | None)
    }

    pub fn peek_token_is(&mut self, token: &Token) -> bool {
        self.peek_token().as_ref() == Some(token)
    }

    pub fn indent(&mut self) {
        self.indent += 1;
    }

    pub fn unindent(&mut self) {
        self.indent -= 1;
    }
}
