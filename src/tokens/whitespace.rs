use lexer::{Input, Reader, ReaderResult, Readers, State};
use crate::{Token, TokenError};
use crate::tokens::utils::{is_closer, is_whitespace};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct WhitespaceReader;

impl Reader<Token, TokenError> for WhitespaceReader {
  fn read(
    &self,
    _: &Readers<Token, TokenError>,
    input: &mut dyn Input,
    _: &State,
    next: &mut State,
  ) -> ReaderResult<Token, TokenError> {
    match input.read(next) {
      Some(ch) => {
        if is_whitespace(ch) {
          while let Some(ch) = input.peek(next, 0) {
            if is_whitespace(ch) {
              input.read(next);
            } else {
              break;
            }
          }

          ReaderResult::Empty
        } else {
          ReaderResult::None
        }
      }
      None => ReaderResult::None,
    }
  }
}
