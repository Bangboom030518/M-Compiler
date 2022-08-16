pub fn is_whitespace(ch: char) -> bool {
    ch.is_whitespace() || ch == ','
}

pub fn is_closer(ch: char) -> bool {
    ch == ')'
}
  