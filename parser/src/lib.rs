use std::iter::Peekable;
use tokenizer::{Token, Tokenizer};

pub struct TopLevelParser<'a>(Peekable<Tokenizer<'a>>);

impl<'a> TopLevelParser<'a> {
    
}

impl<'a> Iterator for TopLevelParser<'a> {
    type Item = TopLevelDeclaration;

    fn next(&mut self) -> Option<Self::Item> {
        // TODO: errors!
        match self.0.next()? {
            Token::Type => {
                let Token::Ident(ident) = self.0.next()? else {
                    return None;
                };
                let ident = Identifier(ident);
                // TODO: whitespace?
                if self.0.next()? != Token::Assignment {
                    return None;
                }

            }
            Token::Function => todo!(),
            Token::Const => todo!(),
            Token::Comment(_) | Token::Newline => self.next(),
            _ => None,
        }
    }
}

pub struct Identifier(String);

impl Identifier {
    // TODO: errors!
    fn parse(input: &mut Tokenizer) -> Option<Self> {
        // TODO: parse Type
        let Token::Ident(ident) = input.next()? else {
            return None;
        };
        Some(Self(ident))
    }
}

pub enum Type {
    Identifier(Identifier),
}

impl Type {
    // TODO: errors!
    fn parse(input: &mut Tokenizer) -> Option<Self> {
        Some(Self::Identifier(Identifier::parse(input)?))
    }
}

// TODO: rename
struct Field {
    r#type: Type,
    name: Identifier,
}

impl Field {
    // TODO: errors!
    fn parse(input: &mut Tokenizer) -> Option<Self> {
        // TODO: parse Type
        let r#type = Type::parse(input)?;
        let name = Identifier::parse(input)?;
        Some(Self { r#type, name })
    }
}

pub struct Union {
    variants: Vec<Field>,
    declarations: Vec<TopLevelDeclaration>,
}

impl Union {
    fn parse(input: &mut Tokenizer) -> Option<Self> {
        // TODO: parse Type
        let r#variants = Field::parse(input)?;
        // let name = Identifier::parse(input)?;
        // Some(Self { r#type, name })
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
