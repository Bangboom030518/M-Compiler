use lexer::{Input, Reader, ReaderResult, Readers, State, TokenMeta};
use crate::{Token, TokenError, TokenValue};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct StringReader;

impl Reader<Token, TokenError> for StringReader {
  fn read(
    &self,
    _: &Readers<Token, TokenError>,
    input: &mut dyn Input,
    current: &State,
    next: &mut State,
  ) -> ReaderResult<Token, TokenError> {
    match input.read(next) {
      Some(ch) => {
        if ch == '"' {
          let mut string = String::new();

          while let Some(ch) = input.read(next) {
            if ch == '"' {
              break;
            } else {
              string.push(ch);
            }
          }

          ReaderResult::Some(Token::new(
            TokenMeta::new_state_meta(current, next),
            TokenValue::String(string),
          ))
        } else {
          ReaderResult::None
        }
      }
      None => ReaderResult::None,
    }
  }
}