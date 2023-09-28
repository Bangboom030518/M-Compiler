use crate::internal::prelude::*;

#[derive(PartialEq, Debug)]
pub enum Type {
    Identifier(Identifier),
}

impl Parse for Type {
    fn parse<'a>(parser: &mut Parser<'a>, _: parser::Marker) -> Option<Self>
    where
        Self: Sized,
    {
        Some(Self::Identifier(parser.parse()?))
    }
}

#[derive(PartialEq, Debug)]
// TODO: rename
struct Field {
    r#type: Type,
    name: Identifier,
}

impl Parse for Field {
    fn parse<'a>(parser: &mut Parser<'a>, _: parser::Marker) -> Option<Self>
    where
        Self: Sized,
    {
        Some(Self {
            r#type: parser.parse()?,
            name: parser.parse()?,
        })
    }
}

#[derive(PartialEq, Debug)]
pub struct Union {
    variants: Vec<Field>,
    declarations: Vec<Declaration>,
}

impl Parse for Union {
    fn parse<'a>(parser: &mut Parser<'a>, _: parser::Marker) -> Option<Self>
    where
        Self: Sized,
    {
        parser.expect_next_token(&Token::Union).then_some(())?;
        parser.expect_next_token(&Token::Newline).then_some(())?;
        parser.indent();
        let mut variants = Vec::new();
        // TODO: refactor to iter
        while let Some(input) = parser.parse_line() {
            variants.push(input);
        }

        dbg!(&parser);

        // TODO: parse Type
        Some(Self {
            variants,
            // TODO: parse declarations
            declarations: Vec::new(),
        })
    }
}

#[derive(PartialEq, Debug)]
struct Function {
    parameters: Vec<Field>,
    return_type: Type,
    body: Vec<Expression>,
}

#[derive(PartialEq, Debug)]
enum DeclarationKind {
    Function(Function),
    Const(Expression),
    Union(Union),
}

#[derive(PartialEq, Debug)]
struct Declaration {
    name: Identifier,
    kind: DeclarationKind,
}

#[test]
fn union_parses() {
    let source = r"union
    String a
    String b
";
    assert_eq!(
        Parser::from(Tokenizer::from(source))
            .parse::<Union>()
            .unwrap(),
        Union {
            variants: vec![
                Field {
                    r#type: Type::Identifier(Identifier(String::from("String"))),
                    name: Identifier(String::from("a")),
                },
                Field {
                    r#type: Type::Identifier(Identifier(String::from("String"))),
                    name: Identifier(String::from("b")),
                }
            ],
            declarations: vec![],
        }
    );
}
