use crate::parser::{Error, Parser};
use crate::{scope, Ident, Parse, Statement, Type};
use std::iter;
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
        let r#type = parser.parse()?;

        if peek(parser) {
            let name = match r#type.value {
                Type::Ident(ident) => ident.spanned(r#type.span),
            };
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
pub struct Struct {
    pub name: Spanned<Ident>,
    pub fields: Vec<Spanned<Field>>,
    pub scope: scope::Id,
}

impl Parse for Struct {
    fn parse(parser: &mut Parser) -> Result<Spanned<Self>, Error> {
        let span_start = parser.take_token_if(TokenType::Type)?.start();
        let name = parser.parse()?;
        parser.take_token_if(TokenType::Struct)?;
        let scope_id = parser.create_scope();
        
        let mut fields = Vec::new();
        loop {
            dbg!();

            let Ok(field) = parser.parse() else { break };
            fields.push(field);
            if parser.take_token_if(TokenType::Comma).is_err() {
                break;
            }
        }
        
        let span_end = parser.take_token_if(TokenType::End)?.end();
        parser.exit_scope();

        Ok(Self {
            name,
            fields,
            scope: scope_id,
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
        todo!()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: Spanned<Ident>,
    pub parameters: Vec<Spanned<Parameter>>,
    pub return_type: Option<Spanned<Type>>,
    pub body: Vec<Spanned<Statement>>,
}

impl Parse for Function {
    fn parse(parser: &mut Parser) -> Result<Spanned<Self>, Error> {
        let start = parser.take_token_if(TokenType::Function)?.start();
        let mut return_type = parser.parse().ok();

        let name = parser.parse().or_else({
            |err| match return_type.clone() {
                Some(Spanned {
                    value: Type::Ident(ident),
                    span,
                }) => {
                    return_type = None;
                    Ok(ident.spanned(span))
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
                _ => todo!("Invalid intrinsic error"),
            };
            Ok(kind)
        })
    }
}

impl Parse for Primitive {
    fn parse(parser: &mut Parser) -> Result<Spanned<Self>, Error> {
        let start = parser.take_token_if(TokenType::Type)?.start();
        let name = parser.parse()?;
        parser.take_token_if(TokenType::At)?;
        let kind = parser.parse()?;
        let end = parser.take_token_if(TokenType::End)?.end();

        Ok(Self { kind, name }.spanned(start..end))
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
        let start = parser.take_token_if(TokenType::Function)?.start();
        let name = parser.parse()?;
        parser.take_token_if(TokenType::At)?;
        if parser.parse::<Ident>()?.value.0 != "extern" {
            todo!()
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
            .or_else(|_| parser.parse().map_spanned(Self::ExternFunction))
            .or_else(|_| parser.parse().map_spanned(Self::Struct))
            .or_else(|_| parser.parse().map_spanned(Self::Primitive))
    }
}

#[test]
fn test_primitive() {
    use tokenizer::Tokenizer;

    let source = r#"type U32 @u32 end"#;
    let primitive = Parser::from(Tokenizer::from(source))
        .parse::<Primitive>()
        .expect("didn't parse!");

    assert_eq!(
        primitive,
        Primitive {
            kind: PrimitiveKind::U32.spanned(9..11),
            name: Ident(String::from("U32")).spanned(5..8),
        }
        .spanned(0..source.len())
    );
}

#[test]
fn top_level_decl_parses() {
    use tokenizer::Tokenizer;

    let source = r"type Point struct
    UInt8 x
    UInt8 y
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
                scope: _,
            }),
        ..
    } = declaration
    else {
        panic!()
    };

    assert_eq!(name, "Point");

    let [Spanned {
        value:
            Field {
                r#type:
                    Spanned {
                        value: Type::Ident(Ident(field_1_ty)),
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
                        value: Type::Ident(Ident(field_2_ty)),
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
