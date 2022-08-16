use lexer::{Input, Reader, ReaderResult, Readers, State, TokenMeta};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct NumberReader;

impl Reader<Token, TokenError> for NumberReader {
  fn read(
    &self,
    _: &Readers<Token, TokenError>,
    input: &mut dyn Input,
    current: &State,
    next: &mut State,
  ) -> ReaderResult<Token, TokenError> {
    match input.read(next) {
      Some(ch) => {
        if ch.is_numeric() || ch == '-' {
          let mut string = String::new();

          string.push(ch);

          while let Some(ch) = input.peek(next, 0) {
            if ch.is_numeric() || ch == '_' {
              input.read(next);
              string.push(ch);
            } else {
              break;
            }
          }

          ReaderResult::Some(Token::new(
            TokenMeta::new_state_meta(current, next),
            TokenValue::Number(string.parse().unwrap()),
          ))
        } else {
          ReaderResult::None
        }
      }
      None => ReaderResult::None,
    }
  }
}
