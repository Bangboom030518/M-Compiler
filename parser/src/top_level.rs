use crate::parser::{Branch, Error, Parser, Recoverable};
use crate::{Ident, Parse, Statement, Type};
use tokenizer::{AsSpanned, Spanned, SpannedResultExt, TokenType};

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct TypeBinding {
    pub r#type: Option<Spanned<Type>>,
    pub name: Spanned<Ident>,
}

impl TypeBinding {
    fn parse(
        parser: &mut Parser,
        peek: impl Fn(&mut Parser) -> bool,
    ) -> Result<Spanned<Self>, Error> {
        let r#type = parser.parse::<Type>()?;

        if peek(parser) && r#type.value.generics.value.is_empty() {
            let name = r#type.value.name;
            let span = name.span.clone();
            Ok(Self { name, r#type: None }.spanned(span))
        } else {
            let name = parser.parse()?;
            let span = r#type.start()..name.end();
            Ok(Self {
                r#type: Some(r#type),
                name,
            }
            .spanned(span))
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Field {
    pub r#type: Spanned<Type>,
    pub name: Spanned<Ident>,
}

impl Parse for Field {
    fn parse(parser: &mut Parser) -> Result<Spanned<Self>, Error> {
        let r#type = parser.parse()?;
        let name = parser.parse()?;
        let span = r#type.start()..name.end();
        Ok(Self { r#type, name }.spanned(span))
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Parameter(pub TypeBinding);

impl Parse for Parameter {
    fn parse(parser: &mut Parser) -> Result<Spanned<Self>, Error> {
        TypeBinding::parse(parser, |parser| {
            parser.peek_token_if(TokenType::Comma).is_ok()
        })
        .map(|binding| Self(binding.value).spanned(binding.span))
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Generic {
    Type { name: Spanned<Ident> },
    Length { name: Spanned<Ident> },
}

impl Parse for Generic {
    fn parse(parser: &mut Parser) -> Result<Spanned<Self>, Error> {
        let value = if let Ok(at) = parser.take_token_if(TokenType::At) {
            if parser.take_ident()?.value != "length" {
                return Err(parser.unexpected_ident(&["length"]));
            }
            let name = parser.parse()?;
            let end = name.end();
            Self::Length { name }.spanned(at.start()..end)
        } else {
            let name = parser.parse()?;
            let span = name.span.clone();
            Self::Type { name }.spanned(span)
        };
        Ok(value)
    }
}

#[derive(PartialEq, Eq, Debug, Default, Clone)]
pub struct Generics {
    pub generics: Vec<Spanned<Generic>>,
}

impl Parse for Generics {
    fn parse(parser: &mut Parser) -> Result<Spanned<Self>, Error> {
        let Ok(open) = parser.take_token_if(TokenType::OpenSquareParen) else {
            return Ok(Self {
                generics: Vec::new(),
            }
            .spanned(parser.empty_span()));
        };
        let generics = parser.parse_csv();
        Ok(Self { generics }
            .spanned(open.start()..parser.take_token_if(TokenType::CloseSquareParen)?.end()))
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Struct {
    pub name: Spanned<Ident>,
    pub generics: Spanned<Generics>,
    pub fields: Vec<Spanned<Field>>,
}

impl Parse for Struct {
    fn parse(parser: &mut Parser) -> Result<Spanned<Self>, Error> {
        let span_start = parser.take_token_if(TokenType::Type)?.start();
        let generics = parser.parse()?;
        let name = parser.parse()?;
        parser.take_token_if(TokenType::Struct)?;

        let mut fields = Vec::new();
        loop {
            let Ok(field) = parser.parse() else { break };
            fields.push(field);
            if parser.take_token_if(TokenType::Comma).is_err() {
                break;
            }
        }

        let span_end = parser.take_token_if(TokenType::End)?.end();

        Ok(Self {
            name,
            generics,
            fields,
        }
        .spanned(span_start..span_end))
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Union {
    pub name: Spanned<Ident>,
}

impl Parse for Union {
    fn parse(_: &mut Parser) -> Result<Spanned<Self>, Error> {
        todo!("unions")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: Spanned<Ident>,
    pub generics: Spanned<Generics>,
    pub parameters: Vec<Spanned<Parameter>>,
    pub return_type: Option<Spanned<Type>>,
    pub body: Vec<Spanned<Statement>>,
}

impl Parse for Function {
    fn parse(parser: &mut Parser) -> Result<Spanned<Self>, Error> {
        let start = parser
            .take_token_if(TokenType::Function)
            .recoverable()?
            .start();
        let generics = parser.parse()?;
        let mut return_type = parser.parse().ok();

        let name = parser.parse().or_else({
            |err| match return_type.clone() {
                Some(Spanned {
                    value: Type { name, generics },
                    ..
                }) if generics.value.is_empty() => {
                    return_type = None;
                    Ok(name)
                }
                _ => Err(err),
            }
        })?;

        parser.take_token_if(TokenType::OpenParen)?;
        let parameters = parser.parse_csv();
        parser.take_token_if(TokenType::CloseParen)?;

        let mut body = Vec::new();
        while let Ok(input) = parser.parse() {
            body.push(input);
        }

        let end = parser.take_token_if(TokenType::End)?.end();

        Ok(Self {
            name,
            generics,
            parameters,
            return_type,
            body,
        }
        .spanned(start..end))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Primitive {
    pub kind: Spanned<PrimitiveKind>,
    pub generics: Spanned<Generics>,
    pub name: Spanned<Ident>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PrimitiveKind {
    U8,
    U16,
    U32,
    U64,
    U128,
    I8,
    I16,
    I32,
    I64,
    I128,
    F32,
    F64,
    USize,
    Array(Spanned<Length>, Spanned<Type>),
}

impl PrimitiveKind {
    const VALID_IDENTS: &'static [&'static str] = &[
        "i8", "i16", "i32", "i64", "i128", "u8", "u16", "u32", "u64", "u128", "usize", "array",
    ];
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Length {
    Literal(u128),
    Ident(Ident),
}

impl Parse for Length {
    fn parse(parser: &mut Parser) -> Result<Spanned<Self>, Error> {
        parser
            .parse()
            .map_spanned(Self::Ident)
            .or_else(|_| parser.take_integer().map_spanned(Self::Literal))
    }
}

impl Parse for PrimitiveKind {
    fn parse(parser: &mut Parser) -> Result<Spanned<Self>, Error> {
        parser.take_ident().and_then_spanned(|ident| {
            let kind = match ident.as_str() {
                "i8" => Self::I8,
                "i16" => Self::I16,
                "i32" => Self::I32,
                "i64" => Self::I64,
                "i128" => Self::I128,
                "u8" => Self::U8,
                "u16" => Self::U16,
                "u32" => Self::U32,
                "u64" => Self::U64,
                "u128" => Self::U128,
                "f32" => Self::F32,
                "f64" => Self::F64,
                "usize" => Self::USize,
                "array" => {
                    parser.take_token_if(TokenType::OpenParen)?;
                    let length = parser.parse()?;
                    parser.take_token_if(TokenType::Comma)?;
                    let inner = parser.parse()?;
                    parser.take_token_if(TokenType::CloseParen)?;
                    Self::Array(length, inner)
                }
                _ => return Err(parser.unexpected_ident(Self::VALID_IDENTS)),
            };
            Ok(kind)
        })
    }
}

impl Parse for Primitive {
    fn parse(parser: &mut Parser) -> Result<Spanned<Self>, Error> {
        let start = parser.take_token_if(TokenType::Type)?.start();
        let generics = parser.parse()?;
        let name = parser.parse()?;
        parser.take_token_if(TokenType::At).recoverable()?;
        let kind = parser.parse()?;
        let end = parser.take_token_if(TokenType::End)?.end();
        Ok(Self {
            kind,
            generics,
            name,
        }
        .spanned(start..end))
    }
}

#[derive(Debug, Clone)]
pub struct ExternFunction {
    pub name: Spanned<Ident>,
    pub symbol: Spanned<String>,
    pub parameters: Vec<Spanned<Type>>,
    pub return_type: Spanned<Type>,
}

impl Parse for ExternFunction {
    fn parse(parser: &mut Parser) -> Result<Spanned<Self>, Error> {
        let start = parser
            .take_token_if(TokenType::Function)
            .recoverable()?
            .start();
        let name = parser.parse()?;
        parser.take_token_if(TokenType::At)?;
        if parser.parse::<Ident>()?.value.0 != "extern" {
            return Err(parser.unexpected_ident(&["extern"]));
        };

        parser.take_token_if(TokenType::OpenParen)?;
        let symbol = parser.take_string()?;

        parser.take_token_if(TokenType::Comma)?;
        parser.take_token_if(TokenType::Function)?;

        parser.take_token_if(TokenType::OpenParen)?;
        let parameters = parser.parse_csv();
        parser.take_token_if(TokenType::CloseParen)?;

        let return_type = parser.parse()?;
        let _ = parser.take_token_if(TokenType::Comma);

        let end = parser.take_token_if(TokenType::CloseParen)?.end();

        Ok(Self {
            name,
            symbol,
            parameters,
            return_type,
        }
        .spanned(start..end))
    }
}

#[derive(Debug, Clone)]
pub enum Declaration {
    Function(Function),
    ExternFunction(ExternFunction),
    Union(Union),
    Struct(Struct),
    Primitive(Primitive),
}

impl Declaration {
    #[must_use]
    pub fn name(&self) -> &str {
        match self {
            Self::Function(value) => value.name.value.0.as_str(),
            Self::ExternFunction(value) => value.name.value.0.as_str(),
            Self::Struct(value) => value.name.value.0.as_str(),
            Self::Union(value) => value.name.value.0.as_str(),
            Self::Primitive(value) => value.name.value.0.as_str(),
        }
    }
}

impl Parse for Declaration {
    fn parse(parser: &mut Parser) -> Result<Spanned<Self>, Error> {
        parser
            .parse()
            .map_spanned(Self::Function)
            .branch(parser, Self::ExternFunction)
            .branch(parser, Self::Struct)
            .branch(parser, Self::Primitive)
    }
}

#[test]
fn test_primitive() {
    use tokenizer::Tokenizer;

    let source = "type U32 @u32 end";
    let primitive = Parser::from(Tokenizer::from(source))
        .parse::<Primitive>()
        .expect("didn't parse!");

    assert_eq!(
        primitive,
        Primitive {
            kind: PrimitiveKind::U32.spanned(9..11),
            generics: Generics::default().spanned(4..4),
            name: Ident(String::from("U32")).spanned(5..8),
        }
        .spanned(0..source.len())
    );
}

#[test]
fn top_level_decl_parses() {
    use tokenizer::Tokenizer;

    let source = r"type Point struct
    UInt8 x,
    UInt8 y,
end";

    let declaration = Parser::from(Tokenizer::from(source))
        .parse::<Declaration>()
        .expect("parse error");

    let Spanned {
        value:
            Declaration::Struct(Struct {
                fields,
                name: Spanned {
                    value: Ident(name), ..
                },
                generics: Spanned {
                    value: generics, ..
                },
            }),
        ..
    } = declaration
    else {
        panic!()
    };

    assert_eq!(name, "Point");
    assert_eq!(generics, Generics::default());

    let [Spanned {
        value:
            Field {
                r#type:
                    Spanned {
                        value:
                            Type {
                                name:
                                    Spanned {
                                        value: Ident(field_1_ty),
                                        ..
                                    },
                                generics: field_1_generics,
                            },
                        ..
                    },
                name:
                    Spanned {
                        value: Ident(field_1),
                        ..
                    },
            },
        ..
    }, Spanned {
        value:
            Field {
                r#type:
                    Spanned {
                        value:
                            Type {
                                name:
                                    Spanned {
                                        value: Ident(field_2_ty),
                                        ..
                                    },
                                generics: field_2_generics,
                            },
                        ..
                    },
                name:
                    Spanned {
                        value: Ident(field_2),
                        ..
                    },
            },
        ..
    }] = fields.as_slice()
    else {
        panic!()
    };
    assert!(field_1_generics.value.is_empty());
    assert!(field_2_generics.value.is_empty());
    assert_eq!(field_1, "x");
    assert_eq!(field_1_ty, "UInt8");
    assert_eq!(field_2, "y");
    assert_eq!(field_2_ty, "UInt8");
}

#[cfg(ignore)]
#[test]
fn function_parses() {
    use tokenizer::Tokenizer;

    // TODO: fails!
    let source = r"fn a(String a, b) UInt32
    a
    a
end";
    assert_eq!(
        Parser::from(Tokenizer::from(source))
            .parse::<Function>()
            .unwrap(),
        Function {
            parameters: vec![
                Parameter(TypeBinding {
                    r#type: Some(Type::Ident(Ident(String::from("String")))),
                    name: Ident(String::from("a")),
                }),
                Parameter(TypeBinding {
                    r#type: None,
                    name: Ident(String::from("b")),
                }),
            ],
            return_type: Some(Type::Ident(Ident(String::from("UInt32")))),
            body: vec![
                Statement::Expression(Expression::Ident(Ident(String::from("a")))),
                Statement::Expression(Expression::Ident(Ident(String::from("a"))))
            ],
        }
    );
}

pub mod prelude {
    pub use super::{Function, Struct, Union};
}
