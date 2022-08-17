extern crate lexer;

use lexer::ReadersBuilder;
use std::fmt::{self, Debug, Write};
mod tokens;
use tokens::{
    IdentifierReader, KeywordReader, ListReader, NumberReader, StringReader, WhitespaceReader,
};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum TokenValue {
    Number(isize),
    String(String),
    Keyword(String),
    Identifier(String),
    List(Vec<Token>),
    OpenBracket,
    CloseBracket,
}

impl fmt::Display for TokenValue {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            TokenValue::Number(ref value) => write!(formatter, "{}", value),
            TokenValue::String(ref value) => write!(formatter, "{:?}", value),
            TokenValue::Keyword(ref value) => write!(formatter, ":{}", value),
            TokenValue::Identifier(ref value) => write!(formatter, "{}", value),
            TokenValue::OpenBracket => write!(formatter, "("),
            TokenValue::CloseBracket => write!(formatter, ")"),
            TokenValue::List(ref list) => {
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

fn readers() -> lexer::Readers<Token, TokenError> {
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

    let string = "def-fn hello () (println :Hello, \"World!\")";

    let tokens = readers.read(string.chars());
    print_tokens(tokens.map(Result::unwrap).collect());
}

fn print_tokens(tokens: Vec<Token>) {
    for token in tokens {
        if let TokenValue::List(values) = token.value() {
            print_tokens(values.to_owned());
        } else {
          println!("{}", token.value());
        }
    }
}
