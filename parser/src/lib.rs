use itertools::{Itertools, PeekingNext};
use tokenizer::{Token, Tokenizer};

trait FromTokens {
    // TODO: errors!
    fn from_tokens<I>(tokens: &mut I) -> Option<Self>
    where
        I: Iterator<Item = Token> + PeekingNext,
        Self: Sized;
}

trait Parse {
    fn parse<T: FromTokens>(&mut self) -> Option<T>;
}

impl<I> Parse for I
where
    I: Iterator<Item = Token> + PeekingNext,
{
    fn parse<T: FromTokens>(&mut self) -> Option<T> {
        T::from_tokens(self)
    }
}

// pub struct TopLevelParser<I: Iterator<Item = Token> + PeekingNext>(I);

// impl<I> Iterator for TopLevelParser<I>
// where
//     I: Iterator<Item = Token> + PeekingNext,
// {
//     type Item = TopLevelDeclaration;

//     fn next(&mut self) -> Option<Self::Item> {
//         // TODO: errors!
//         match self.0.next()? {
//             Token::Type => {
//                 let Token::Ident(ident) = self.0.next()? else {
//                     return None;
//                 };
//                 let ident = Identifier(ident);
//                 // TODO: whitespace?
//                 if self.0.next()? != Token::Assignment {
//                     return None;
//                 }
//             }
//             Token::Function => todo!(),
//             Token::Const => todo!(),
//             Token::Comment(_) | Token::Newline => self.next(),
//             _ => None,
//         }
//     }
// }

pub struct Identifier(String);

impl FromTokens for Identifier {
    fn from_tokens<I>(tokens: &mut I) -> Option<Self>
    where
        I: Iterator<Item = Token> + PeekingNext,
    {
        let Token::Ident(ident) = tokens.next()? else {
            return None;
        };
        Some(Self(ident))
    }
}

pub enum Type {
    Identifier(Identifier),
}

impl FromTokens for Type {
    fn from_tokens<I>(tokens: &mut I) -> Option<Self>
    where
        I: Iterator<Item = Token> + PeekingNext,
    {
        Some(Self::Identifier(tokens.parse()?))
    }
}

// TODO: rename
struct Field {
    r#type: Type,
    name: Identifier,
}

impl FromTokens for Field {
    fn from_tokens<I>(tokens: &mut I) -> Option<Self>
    where
        I: Iterator<Item = Token> + PeekingNext,
    {
        Some(Self {
            r#type: tokens.parse()?,
            name: tokens.parse()?,
        })
    }
}

pub struct Union {
    variants: Vec<Field>,
    declarations: Vec<TopLevelDeclaration>,
}

impl FromTokens for Union {
    fn from_tokens<I>(tokens: &mut I) -> Option<Self>
    where
        I: Iterator<Item = Token> + PeekingNext,
    {
        if tokens.next()? != Token::Union {
            return None;
        }
        let mut variants = Vec::new();
        while let Some(input) = tokens.parse() {
            variants.push(input)
        }
        // TODO: parse Type
        Some(Self { variants, declarations: todo!() })
    }
}

struct TypeDeclaration {
    fields: Vec<Field>,
    declarations: Vec<TopLevelDeclaration>,
    kind: TypeDeclarationKind,
}

struct Function {
    parameters: Vec<Field>,
    return_type: Type,
    body: Vec<Expression>,
}

enum Expression {
    Identifier(Identifier),
}

enum TopLevelDeclarationKind {
    Function(Function),
    Const(Expression),
    // TODO: ?
    Type(TypeDeclaration),
}

struct TopLevelDeclaration {
    name: Identifier,
    declaration: TopLevelDeclarationKind,
}

#[test]
fn union_parses() {
    assert_eq!(
        r#"union
    String a
    String b"#
    );
}
