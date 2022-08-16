use lexer::{Input, Reader, ReaderResult, Readers, State, TokenMeta};
use crate::{Token, TokenError, TokenValue};
use crate::tokens::utils::{is_closer, is_whitespace};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct IdentifierReader;

impl Reader<Token, TokenError> for IdentifierReader {
  fn read(
    &self,
    _: &Readers<Token, TokenError>,
    input: &mut dyn Input,
    current: &State,
    next: &mut State,
  ) -> ReaderResult<Token, TokenError> {
    match input.read(next) {
      Some(ch) => {
        let mut string = String::new();

        string.push(ch);

        while let Some(ch) = input.peek(next, 0) {
          if is_closer(ch) || is_whitespace(ch) {
            break;
          } else {
            input.read(next);
            string.push(ch);
          }
        }

        ReaderResult::Some(Token::new(
          TokenMeta::new_state_meta(current, next),
          TokenValue::Identifier(string),
        ))
      }
      None => ReaderResult::None,
    }
  }
}
