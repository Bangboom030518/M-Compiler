use lexer::{Input, Reader, ReaderResult, Readers, State, TokenMeta};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct ListReader;

impl Reader<Token, TokenError> for ListReader {
  fn read(
    &self,
    readers: &Readers<Token, TokenError>,
    input: &mut dyn Input,
    current: &State,
    next: &mut State,
  ) -> ReaderResult<Token, TokenError> {
    match input.read(next) {
      Some(ch) => {
        if ch == '(' {
          let mut list = Vec::new();

          while let Some(ch) = input.peek(next, 0) {
            if ch == ')' {
              input.read(next);
              break;
            } else {
              match lexer::read(readers, input, next) {
                Some(Ok(token)) => {
                  list.push(token);
                }
                Some(Err(error)) => {
                  return ReaderResult::Err(error);
                }
                _ => {
                  break;
                }
              }
            }
          }

          ReaderResult::Some(Token::new(
            TokenMeta::new_state_meta(current, next),
            TokenValue::List(list),
          ))
        } else {
          ReaderResult::None
        }
      }
      None => ReaderResult::None,
    }
  }
}
