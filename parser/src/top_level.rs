use crate::internal::prelude::*;
use crate::Error;
use std::iter;
use tokenizer::TokenType;

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct TypeBinding {
    pub r#type: Option<Type>,
    pub name: Ident,
}

impl TypeBinding {
    fn parse(parser: &mut Parser, peek: impl Fn(&mut Parser) -> bool) -> Result<Self, Error> {
        let r#type = parser.parse()?;
        if peek(parser) {
            Ok(Self {
                name: match r#type {
                    Type::Ident(ident) => ident,
                    // _ => return None,
                },
                r#type: None,
            })
        } else {
            Ok(Self {
                r#type: Some(r#type),
                name: parser.parse()?,
            })
        }
    }
}

impl Spanned for TypeBinding {
    fn span(&self) -> tokenizer::Span {
        self.r#type
            .map(|ty| ty.start())
            .unwrap_or_else(|| self.name.start())..self.name.end()
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Spanned)]
pub struct Field {
    #[span(start)]
    pub r#type: Type,
    #[span(end)]
    pub name: Ident,
}

impl Parse for Field {
    fn parse(parser: &mut Parser) -> Result<Self, Error> {
        Ok(Self {
            r#type: parser.parse()?,
            name: parser.parse()?,
        })
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Spanned)]
pub struct Parameter(#[span] pub TypeBinding);

impl Parse for Parameter {
    fn parse(parser: &mut Parser) -> Result<Self, Error> {
        TypeBinding::parse(parser, |parser| {
            parser.peek_token_if(TokenType::Comma).is_ok()
        })
        .map(Self)
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Spanned)]
pub struct Struct {
    pub name: Ident,
    pub fields: Vec<Field>,
    pub scope: scope::Id,
    #[span]
    span: tokenizer::Span,
}

impl Parse for Struct {
    fn parse(parser: &mut Parser) -> Result<Self, Error> {
        let span_start = parser.take_token_if(TokenType::Type)?.start();
        let name = parser.parse()?;
        parser.take_token_if(TokenType::Struct)?;
        let scope_id = parser.create_scope();

        let fields = iter::from_fn(|| parser.parse().ok()).collect();

        let span_end = parser.take_token_if(TokenType::End)?.end();
        parser.exit_scope();

        Ok(Self {
            name,
            fields,
            scope: scope_id,
            span: span_start..span_end,
        })
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Spanned)]
pub struct Union {
    pub name: Ident,
    #[span]
    span: tokenizer::Span,
}

impl Parse for Union {
    fn parse(parser: &mut Parser) -> Result<Self, Error> {
        todo!()
    }
}

#[derive(Debug, Clone, Spanned, PartialEq)]
pub struct Function {
    pub name: Ident,
    pub parameters: Vec<Parameter>,
    pub return_type: Option<Type>,
    pub body: Vec<Statement>,
    #[span]
    span: tokenizer::Span,
}

impl Parse for Function {
    fn parse(parser: &mut Parser) -> Result<Self, Error> {
        let start = parser.take_token_if(TokenType::Function)?.start();
        let mut return_type = parser.parse().ok();

        let name = parser.parse().or_else(|err| {
            return_type = None;
            match return_type {
                Some(Type::Ident(ident)) => Ok(ident),
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
            span: start..end,
        })
    }
}

#[derive(Debug, Clone, Spanned, PartialEq)]
pub struct Primitive {
    pub kind: SpannedPrimitiveKind,
    pub name: Ident,
    #[span]
    pub span: tokenizer::Span,
}

#[derive(Debug, Clone, PartialEq)]
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
    MutablePointer(Type),
    MutableSlice(Type),
}

#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct SpannedPrimitiveKind {
    kind: PrimitiveKind,
    #[span]
    span: tokenizer::Span,
}

impl Parse for SpannedPrimitiveKind {
    fn parse(parser: &mut Parser) -> Result<Self, Error> {
        let (span, ident) = parser.take_ident()?;
        let kind = match ident.as_str() {
            "i8" => PrimitiveKind::I8,
            "i16" => PrimitiveKind::I16,
            "i32" => PrimitiveKind::I32,
            "i64" => PrimitiveKind::I64,
            "i128" => PrimitiveKind::I128,
            "u8" => PrimitiveKind::U8,
            "u16" => PrimitiveKind::U16,
            "u32" => PrimitiveKind::U32,
            "u64" => PrimitiveKind::U64,
            "u128" => PrimitiveKind::U128,
            "f32" => PrimitiveKind::F32,
            "f64" => PrimitiveKind::F64,
            "usize" => PrimitiveKind::USize,
            "mutable_pointer" => {
                parser.take_token_if(TokenType::OpenParen)?;
                let pointer = parser.parse().map(PrimitiveKind::MutablePointer)?;
                parser.take_token_if(TokenType::CloseParen)?;
                pointer
            }
            "mutable_slice" => {
                parser.take_token_if(TokenType::OpenParen)?;
                let pointer = parser.parse().map(PrimitiveKind::MutableSlice)?;
                parser.take_token_if(TokenType::CloseParen)?;
                pointer
            }
            _ => todo!("Invalid intrinsic error"),
        };
        Ok(Self { kind, span })
    }
}

impl Parse for Primitive {
    fn parse(parser: &mut Parser) -> Result<Self, Error> {
        let start = parser.take_token_if(TokenType::Type)?.start();
        let name = parser.parse()?;
        parser.take_token_if(TokenType::At)?;
        let kind = parser.parse()?;
        let end = parser.take_token_if(TokenType::End)?.end();

        Ok(Self {
            name,
            kind,
            span: start..end,
        })
    }
}

#[derive(Debug, Clone, Spanned)]
pub struct ExternFunction {
    pub name: Ident,
    pub symbol: String,
    pub parameters: Vec<Type>,
    pub return_type: Type,
    #[span]
    span: tokenizer::Span,
}

impl Parse for ExternFunction {
    fn parse(parser: &mut Parser) -> Result<Self, Error> {
        let start = parser.take_token_if(TokenType::Function)?.start();
        let name = parser.parse()?;
        parser.take_token_if(TokenType::At)?;
        if parser.parse::<Ident>()?.ident != "extern" {
            todo!()
        };

        parser.take_token_if(TokenType::OpenParen)?;
        let (_, symbol) = parser.take_string()?;

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
            span: start..end,
        })
    }
}

#[derive(Debug, Clone, Spanned)]
pub enum Declaration {
    Function(Function),
    ExternFunction(ExternFunction),
    Union(Union),
    Struct(Struct),
    Primitive(Primitive),
}

impl Declaration {
    pub fn name(&self) -> &str {
        match self {
            Self::Function(value) => value.name.ident.as_str(),
            Self::ExternFunction(value) => value.name.ident.as_str(),
            Self::Struct(value) => value.name.ident.as_str(),
            Self::Union(value) => value.name.ident.as_str(),
            Self::Primitive(value) => value.name.ident.as_str(),
        }
    }
}

impl Parse for Declaration {
    fn parse(parser: &mut Parser) -> Result<Self, Error> {
        parser
            .parse()
            .map(Self::Function)
            .or_else(|_| parser.parse().map(Self::ExternFunction))
            .or_else(|_| parser.parse().map(Self::Union))
            .or_else(|_| parser.parse().map(Self::Struct))
            .or_else(|_| parser.parse().map(Self::Primitive))
    }
}

#[test]
fn test_primitive() {
    let source = r#"type U32 @u32 end"#;
    let primitive = Parser::from(Tokenizer::from(source))
        .parse::<Primitive>()
        .unwrap();
    assert_eq!(
        primitive,
        Primitive {
            kind: SpannedPrimitiveKind {
                kind: PrimitiveKind::U32,
                span: 9..11,
            },
            name: Ident {
                ident: String::from("U32"),
                span: 5..8,
            },
            span: 0..source.len(),
        }
    );
}
#[test]
fn top_level_decl_parses() {
    let uint_8 = Type::Ident(Ident(String::from("UInt8")));
    let source = r"type Point = struct
    UInt8 x
    UInt8 y
end";
    let declaration = Parser::from(Tokenizer::from(source))
        .parse::<Declaration>()
        .unwrap();

    let scope = match declaration.kind {
        DeclarationKind::Struct(ref r#struct) => r#struct.scope,
        _ => panic!(),
    };

    assert_eq!(
        declaration,
        Declaration {
            name: Ident(String::from("Point")),
            kind: DeclarationKind::Struct(Struct {
                fields: vec![
                    Field {
                        r#type: uint_8.clone(),
                        name: Ident(String::from("x"))
                    },
                    Field {
                        r#type: uint_8,
                        name: Ident(String::from("y"))
                    }
                ],
                scope,
            })
        }
    );
}

#[test]
fn union_parses() {
    let source = r"union
    String a
    b
end";
    let union = Parser::from(Tokenizer::from(source))
        .parse::<Union>()
        .unwrap();
    let scope = union.scope;
    assert_eq!(
        union,
        Union {
            variants: vec![
                Variant(TypeBinding {
                    r#type: Some(Type::Ident(Ident(String::from("String")))),
                    name: Ident(String::from("a")),
                }),
                Variant(TypeBinding {
                    r#type: None,
                    name: Ident(String::from("b")),
                })
            ],
            scope,
        }
    );
}

#[test]
fn function_parses() {
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
