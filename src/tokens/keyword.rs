use lexer::{Input, Reader, ReaderResult, Readers, State, TokenMeta};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct KeywordReader;

impl Reader<Token, TokenError> for KeywordReader {
  fn read(
    &self,
    _: &Readers<Token, TokenError>,
    input: &mut dyn Input,
    current: &State,
    next: &mut State,
  ) -> ReaderResult<Token, TokenError> {
    match input.read(next) {
      Some(ch) => {
        if ch == ':' {
          let mut string = String::new();

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
            TokenValue::Keyword(string),
          ))
        } else {
          ReaderResult::None
        }
      }
      None => ReaderResult::None,
    }
  }
}
