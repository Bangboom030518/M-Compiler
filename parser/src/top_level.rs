use crate::internal::prelude::*;

#[derive(PartialEq, Debug)]
pub enum Type {
    Identifier(Identifier),
}

impl Parse for Type {
    fn parse<'a>(parser: &mut Parser<'a>) -> Option<Self> {
        Some(Self::Identifier(parser.parse()?))
    }
}

#[derive(PartialEq, Debug)]
// TODO: rename
struct TypeBinding {
    r#type: Option<Type>,
    name: Identifier,
}

impl TypeBinding {
    fn parse<'a>(parser: &mut Parser, peek: impl Fn(&mut Parser) -> bool) -> Option<Self> {
        let r#type = parser.parse()?;
        if peek(parser) {
            // panic!("NEXT TOKEN IS NEWLINE");
            Some(Self {
                name: match r#type {
                    Type::Identifier(ref ident) => ident.clone(),
                    _ => return None,
                },
                r#type: None,
            })
        } else {
            Some(Self {
                r#type: Some(r#type),
                name: parser.parse()?,
            })
        }
    }
}

#[derive(PartialEq, Debug)]
struct Variant(TypeBinding);

impl Parse for Variant {
    fn parse<'a>(parser: &mut Parser<'a>) -> Option<Self> {
        TypeBinding::parse(parser, |parser| parser.peek_newline_or_eof()).map(Self)
    }
}

#[derive(PartialEq, Debug)]
struct Parameter(TypeBinding);

impl Parse for Parameter {
    fn parse<'a>(parser: &mut Parser<'a>) -> Option<Self> {
        TypeBinding::parse(parser, |parser| parser.peek_token_is(&Token::Comma)).map(Self)
    }
}

#[derive(PartialEq, Debug)]
pub struct Union {
    variants: Vec<Variant>,
    declarations: Vec<Declaration>,
}

impl Parse for Union {
    fn parse<'a>(parser: &mut Parser<'a>) -> Option<Self>
    where
        Self: Sized,
    {
        parser.next_token_is(&Token::Union).then_some(())?;
        parser.next_token_is(&Token::Newline).then_some(())?;
        parser.indent();
        let mut variants = Vec::new();
        // TODO: refactor to iter
        while let Some(input) = parser.parse_line() {
            variants.push(input);
        }
        parser.unindent();
        Some(Self {
            variants,
            // TODO: parse declarations
            declarations: Vec::new(),
        })
    }
}

#[derive(PartialEq, Debug)]
struct Function {
    parameters: Vec<Parameter>,
    return_type: Type,
    body: Vec<Expression>,
}

impl Parse for Function {
    fn parse<'a>(parser: &mut Parser<'a>) -> Option<Self>
    where
        Self: Sized,
    {
        // function a = (Int a, Int b,) Int ->
        parser.next_token_is(&Token::OpenParen).then_some(())?;

        let mut parameters = Vec::new();
        while let Some(parameter) = parser.parse() {
            parameters.push(parameter);
            parser.next_token_is(&Token::Comma);
        }

        parser.next_token_is(&Token::CloseParen).then_some(())?;
        let return_type = parser.parse()?;

        parser.next_token_is(&Token::Arrow).then_some(())?;

        let mut body = Vec::new();
        if parser.next_token_is(&Token::Newline) {
            parser.indent();
            while let Some(input) = parser.parse_line() {
                body.push(input);
            }
        } else {
            body.push(parser.parse()?)
        };
        parser.unindent();
        Some(Self {
            parameters,
            return_type,
            body,
        })
    }
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

// impl Parse for Declaration {
//     fn parse<'a>(parser: &mut Parser<'a>) -> Option<Self> {
//         let name;
//         if parser.next_token_is(&Token::Type) {

//         } else if parser.next_token_is(&Token::Function) {

//         }

//         Self { name, kind }
//     }
// }

#[test]
fn union_parses() {
    let source = r"union
    String a
    b";
    assert_eq!(
        Parser::from(Tokenizer::from(source))
            .parse::<Union>()
            .unwrap(),
        Union {
            variants: vec![
                Variant(TypeBinding {
                    r#type: Some(Type::Identifier(Identifier(String::from("String")))),
                    name: Identifier(String::from("a")),
                }),
                Variant(TypeBinding {
                    r#type: None,
                    name: Identifier(String::from("b")),
                })
            ],
            declarations: vec![],
        }
    );
}

#[test]
fn function_parses() {
    let source = r"(String a, b,) UInt32 ->
    a
    a";
    let mut parser = Parser::from(Tokenizer::from(source));
    let function = parser.parse::<Function>();
    dbg!(parser);
    assert_eq!(
        function.unwrap(),
        Function {
            parameters: vec![
                Parameter(TypeBinding {
                    r#type: Some(Type::Identifier(Identifier(String::from("String")))),
                    name: Identifier(String::from("a")),
                }),
                Parameter(TypeBinding {
                    r#type: None,
                    name: Identifier(String::from("b")),
                }),
            ],
            return_type: Type::Identifier(Identifier(String::from("UInt32"))),
            body: vec![
                Expression::Identifier(Identifier(String::from("a"))),
                Expression::Identifier(Identifier(String::from("a")))
            ]
        }
    );
}
