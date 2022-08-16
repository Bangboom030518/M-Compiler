extern crate lexer;

use std::fmt::{self, Write};

use lexer::ReadersBuilder;

mod tokens;

use tokens::{StringReader, NumberReader, WhitespaceReader, KeywordReader, IdentifierReader, ListReader};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum TokenValue {
  Number(isize),
  String(String),
  Keyword(String),
  Identifier(String),
  List(Vec<Token>),
}

impl fmt::Display for TokenValue {
  fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      &TokenValue::Number(ref value) => write!(formatter, "{}", value),
      &TokenValue::String(ref value) => write!(formatter, "{:?}", value),
      &TokenValue::Keyword(ref value) => write!(formatter, ":{}", value),
      &TokenValue::Identifier(ref value) => write!(formatter, "{}", value),
      &TokenValue::List(ref list) => {
        formatter.write_char('(')?;

        let mut index = 0;

        for token in list {
          write!(formatter, "{}", token.value())?;

          index += 1;
          if index < list.len() {
            formatter.write_str(", ")?;
          }
        }

        formatter.write_char(')')
      }
    }
  }
}

pub type Token = lexer::Token<TokenValue>;
pub type TokenError = lexer::TokenError<&'static str>;

fn is_closer(ch: char) -> bool {
  ch == ')'
}

pub fn readers() -> lexer::Readers<Token, TokenError> {
  ReadersBuilder::new()
    .add(WhitespaceReader)
    .add(NumberReader)
    .add(StringReader)
    .add(KeywordReader)
    .add(ListReader)
    .add(IdentifierReader)
    .build()
}

fn main() {
  let readers = readers();

  let string = "(def-fn hello () (println :Hello, \"World!\"))";

  let tokens = readers.read(string.chars());
  let tokens: Vec<Token> = tokens.map(Result::unwrap).collect();

  assert_eq!(tokens.len(), 1);

  if let Some(&TokenValue::List(ref tokens)) = tokens.get(0).map(Token::value) {
    let first = tokens.first().unwrap();
    assert_eq!(first.meta().col_start(), 1);
    assert_eq!(first.meta().col_end(), 7);
    assert_eq!(first.meta().col_count(), 6);
    assert_eq!(first.meta().line_start(), 1);
    assert_eq!(first.meta().line_end(), 1);
    assert_eq!(first.meta().line_count(), 0);
    assert_eq!(first.meta().len(), 6);
  }
}
