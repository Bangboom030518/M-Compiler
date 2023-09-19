use std::str::Chars;

fn main() {
    println!("Hello, world!");
}

enum Token {
    Const,
    Function,
    Type,
    // TODO: ?
    // Enum,
    Union,
    Struct,
    Assignment,
    Add,
    Subtract,
    Multiply,
    Divide,
    Indent,
    Dot,
    Equality,
    Bang,
    Illegal,
    Ident(String),
    // Parenthesised(Vec<Token>),
}

struct Tokenizer<'a>(Chars<'a>);

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let ch = self.0.next()?;
        match ch {
            ''
        }
        // Some(ch)
    }
}
