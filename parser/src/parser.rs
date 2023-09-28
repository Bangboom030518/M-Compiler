use crate::internal::prelude::*;

pub struct Marker(());

#[derive(Debug)]
pub struct Parser<'a> {
    tokens: Tokenizer<'a>,
    peeked_tokens: Vec<Token>,
    peek_index: usize,
    indent: u8,
}

impl<'a> From<Tokenizer<'a>> for Parser<'a> {
    fn from(tokens: Tokenizer<'a>) -> Self {
        Self {
            tokens,
            peeked_tokens: Vec::new(),
            peek_index: 0,
            indent: 0,
        }
    }
}

impl<'a> Parser<'a> {
    // TODO: clone?
    pub fn peek_token(&mut self) -> Option<Token> {
        let token = self.peeked_tokens.get(self.peek_index).cloned();
        let token = match token {
            token @ Some(_) => token,
            None => {
                self.peeked_tokens.push(self.tokens.next()?);
                self.peeked_tokens.last().cloned()
            }
        };
        self.peek_index += 1;
        token
    }

    /// Clears peek tokens
    fn advance_peeked(&mut self) {
        self.peeked_tokens.clear()
    }

    /// Return to start of peek
    fn reset_peek(&mut self) {
        self.peek_index = 0
    }

    pub fn parse<T>(&mut self) -> Option<T>
    where
        T: Parse,
    {
        match T::parse(self, Marker(())) {
            None => {
                self.reset_peek();
                None
            }
            result @ Some(_) => {
                self.advance_peeked();
                result
            }
        }
    }

    pub fn parse_line<T>(&mut self) -> Option<T>
    where
        T: Parse,
    {
        for _ in 0..self.indent {
            self.expect_next_token(&Token::Indent).then_some(())?;
        }
        let value: T = self.parse()?;
        // TODO: handle EOFs
        self.expect_next_token(&Token::Newline).then_some(())?;
        Some(value)
    }

    pub fn expect_next_token(&mut self, token: &Token) -> bool {
        if self.peek_token().as_ref() == Some(token) {
            self.advance_peeked();
            true
        } else {
            self.reset_peek();
            false
        }
    }

    pub fn indent(&mut self) {
        self.indent += 1;
    }

    pub fn unindent(&mut self) {
        self.indent -= 1;
    }
}
