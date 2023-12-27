import token.*

#[derive(Debug)]
pub struct Tokenizer<'a>(Peekable<Chars<'a>>);

impl<'a> From<&'a str> for Tokenizer<'a> {
    fn from(value: &'a str) -> Self {
        Self(value.chars().peekable())
    }
}

impl<'a> Tokenizer<'a> {
    fn take_ident_or_keyword(&mut self, first_char: char) -> Token {
        let ident: String = std::iter::once(first_char)
            .chain(
                self.0
                    .peeking_take_while(|&ch| ch.is_alphanumeric() || ch == '_'),
            )
            .collect();

        match ident.as_str() {
            "const" => Token::Const,
            "struct" => Token::Struct,
            "union" => Token::Union,
            "function" => Token::Function,
            "type" => Token::Type,
            "if" => Token::If,
            "else" => Token::Else,
            "let" => Token::Let,
            "return" => Token::Return,
            _ => Token::Ident(ident),
        }
    }

    fn take_escaped_char(&mut self) -> Option<char> {
        let ch = match self.0.next()? {
            'n' => '\n',
            't' => '\t',
            'r' => '\r',
            '0' => '\0',
            'u' => {
                if self.0.next()? != '{' {
                    return None;
                }
                let digits: String = self.0.peeking_take_while(|&ch| ch != '}').collect();
                self.0.next().unwrap();
                let code = u32::from_str_radix(&digits, 16).ok()?;
                char::from_u32(code)?
            }
            '"' => '"',
            '\'' => '\'',
            '\\' => '\\',
            _ => return None,
        };
        Some(ch)
    }

    fn take_string(&mut self) -> Token {
        let mut string = String::new();
        while let Some(ch) = self.0.next() {
            let ch = match ch {
                '\\' => match self.take_escaped_char() {
                    Some(ch) => ch,
                    None => return Token::Illegal,
                },
                '"' => break,
                ch => ch,
            };
            string.push(ch);
        }
        Token::String(string)
    }

    fn take_number_base(&mut self, base: u32) -> Token {
        self.0.next();
        let digits: String = self.0.peeking_take_while(|ch| ch.is_digit(base)).collect();
        u128::from_str_radix(&digits, base)
            .map(Token::Integer)
            .unwrap_or_default()
    }

    fn take_number(&mut self, first_char: char) -> Token {
        if first_char == '0' {
            match self.0.peek() {
                Some(&'x' | &'X') => return self.take_number_base(16),
                Some(&'o' | &'O') => return self.take_number_base(8),
                Some(&'b' | &'B') => return self.take_number_base(2),
                _ => {}
            }
        }

        let mut digits: String = std::iter::once(first_char)
            .chain(
                self.0
                    .peeking_take_while(|&ch| ch.is_ascii_digit() || ch == '_'),
            )
            .collect();

        if self.0.peek() != Some(&'.') {
            return digits.parse().map(Token::Integer).unwrap_or_default();
        }
        self.0.next().unwrap();
        digits.push('.');

        self.0
            .peeking_take_while(|&ch| ch.is_ascii_digit() || ch == '_')
            .collect_into(&mut digits);

        digits.parse().map(Token::Float).unwrap_or_default()
    }

    fn take_comment_or_divide(&mut self) -> Token {
        match self.0.peek() {
            Some(&'/') => {
                self.0.next();
                Token::Comment(self.0.peeking_take_while(|&ch| ch != '\n').collect())
            }
            Some(&'*') => {
                self.0.next();
                let mut comment = String::new();
                while let Some(ch) = self.0.next() {
                    if ch == '*' && self.0.peek() == Some(&'/') {
                        self.next();
                        break;
                    }
                    comment.push(ch)
                }
                Token::Comment(comment)
            }
            _ => Token::Divide,
        }
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let ch = self.0.next()?;
        let token = match ch {
            '+' => Token::Plus,
            '-' => Token::Minus,
            '%' => Token::Remainder,
            '.' => Token::Dot,
            ',' => Token::Comma,
            '@' => Token::At,
            '\t' => Token::Indent,
            '!' => {
                if self.0.peek() == Some(&'=') {
                    self.0.next();
                    Token::NotEqual
                } else {
                    Token::Bang
                }
            }
            '>' => {
                if self.0.peek() == Some(&'=') {
                    self.0.next();
                    Token::GreaterThanOrEqual
                } else {
                    Token::GreaterThan
                }
            }
            '<' => {
                if self.0.peek() == Some(&'=') {
                    self.0.next();
                    Token::LessThanOrEqual
                } else {
                    Token::LessThan
                }
            }
            '(' => Token::OpenParen,
            ')' => Token::CloseParen,
            // TODO: `\r\n`?
            '\n' => Token::Newline,
            '"' => self.take_string(),
            '*' => {
                if self.0.peek() == Some(&'*') {
                    self.0.next();
                    Token::Exponent
                } else {
                    Token::Multiply
                }
            }
            '=' => {
                if self.0.peek() == Some(&'=') {
                    self.0.next();
                    Token::Equal
                } else {
                    Token::Assignment
                }
            }
            '/' => self.take_comment_or_divide(),
            ' ' => {
                for _ in 0..3 {
                    let Some(_) = self.0.peeking_next(|&ch| ch == ' ') else {
                        return self.next();
                    };
                }
                Token::Indent
            }
            ch if ch.is_whitespace() => self.next()?,
            ch if ch.is_alphabetic() || ch == '_' => self.take_ident_or_keyword(ch),
            ch if ch.is_numeric() => self.take_number(ch),
            _ => Token::Illegal,
        };
        Some(token)
    }
}

#[test]
fn tokenize_stuff() {
    let input = "+ / * /*Hello World*/ const identifier_test";
    let tokenizer: Tokenizer = input.into();
    let tokens = tokenizer.collect_vec();
    assert_eq!(
        tokens,
        vec![
            Token::Plus,
            Token::Divide,
            Token::Multiply,
            Token::Comment("Hello World".to_string()),
            Token::Const,
            Token::Ident("identifier_test".to_string())
        ]
    );
}
