use tokenizer::TokenType;

use crate::{internal::prelude::*, Error};
use std::iter;

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct TypeBinding {
    pub r#type: Option<Type>,
    pub name: Ident,
}

impl TypeBinding {
    fn parse(parser: &mut Parser, peek: impl Fn(&mut Parser) -> bool) -> Result<Self, Error> {
        let r#type = parser.parse()?;
        if peek(parser) {
            Some(Self {
                name: match r#type {
                    Type::Ident(ident) => ident,
                    // _ => return None,
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

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Variant(TypeBinding);

impl Parse for Variant {
    fn parse(parser: &mut Parser) -> Result<Self, Error> {
        TypeBinding::parse(parser, |parser| parser.peek_newline_or_eof().is_some()).map(Self)
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Field {
    pub r#type: Type,
    pub name: Ident,
}

impl Parse for Field {
    fn parse(parser: &mut Parser) -> Result<Self, Error> {
        Some(Self {
            r#type: parser.parse()?,
            name: parser.parse()?,
        })
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Parameter(pub TypeBinding);

impl Parse for Parameter {
    fn parse(parser: &mut Parser) -> Result<Self, Error> {
        TypeBinding::parse(parser, |parser| {
            parser.peek_token_if(TokenType::Comma).is_some()
        })
        .map(Self)
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Struct {
    pub fields: Vec<Field>,
    // pub declarations: Vec<Declaration>,
    pub scope: scope::Id,
}

impl Parse for Struct {
    fn parse(parser: &mut Parser) -> Result<Self, Error> {
        parser.take_token_if(TokenType::Struct)?;
        parser.take_newline()?;
        let scope_id = parser.create_scope();

        let fields = iter::from_fn(|| parser.parse_line()).collect();

        while let Some(declaration) = parser.parse_line::<Declaration>() {
            parser
                .get_scope(scope_id)
                .declarations
                .insert(declaration.name, declaration.kind);
        }
        parser.take_token_if(TokenType::End)?;
        parser.exit_scope();

        Some(Self {
            fields,
            scope: scope_id,
        })
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Union {
    pub variants: Vec<Variant>,
    pub scope: scope::Id,
}

impl Parse for Union {
    fn parse(parser: &mut Parser) -> Result<Self, Error> {
        parser.take_token_if(TokenType::Union)?;
        parser.take_newline()?;
        let scope_id = parser.create_scope();
        let variants = iter::from_fn(|| parser.parse_line()).collect_vec();

        while let Some(declaration) = parser.parse_line::<Declaration>() {
            parser
                .get_scope(scope_id)
                .declarations
                .insert(declaration.name, declaration.kind);
        }
        parser.take_token_if(TokenType::End)?;
        parser.exit_scope();

        Some(Self {
            variants,
            scope: scope_id,
        })
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub parameters: Vec<Parameter>,
    pub return_type: Option<Type>,
    pub body: Vec<Statement>,
}

impl Parse for Function {
    fn parse(parser: &mut Parser) -> Result<Self, Error> {
        parser.take_token_if(TokenType::OpenParen)?;

        let parameters = parser.parse_csv();

        parser.take_token_if(TokenType::CloseParen)?;

        let return_type = parser.parse();
        let mut body = Vec::new();
        parser.take_newline()?;

        while let Some(input) = parser.parse_line() {
            body.push(input);
        }
        parser.take_token_if(TokenType::End)?;

        Some(Self {
            parameters,
            return_type,
            body,
        })
    }
}

#[derive(Debug, Clone)]
pub struct Primitive {
    pub kind: PrimitiveKind,
    pub scope: scope::Id,
}

#[derive(Debug, Clone)]
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

impl Parse for PrimitiveKind {
    fn parse(parser: &mut Parser) -> Result<Self, Error> {
        let Token::Ident(ident) = parser.take_token()? else {
            return None;
        };
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
            "mutable_pointer" => {
                parser.take_token_if(TokenType::OpenParen)?;
                let pointer = parser.parse().map(Self::MutablePointer)?;
                parser.take_token_if(TokenType::CloseParen)?;
                pointer
            }
            "mutable_slice" => {
                parser.take_token_if(TokenType::OpenParen)?;
                let pointer = parser.parse().map(Self::MutableSlice)?;
                parser.take_token_if(TokenType::CloseParen)?;
                pointer
            }
            _ => return None,
        };
        Some(kind)
    }
}

impl Parse for Primitive {
    fn parse(parser: &mut Parser) -> Result<Self, Error> {
        parser.take_token_if(TokenType::At)?;
        let kind = parser.parse()?;
        parser.take_newline()?;
        let scope_id = parser.create_scope();
        while let Some(declaration) = parser.parse_line::<Declaration>() {
            parser
                .get_scope(scope_id)
                .declarations
                .insert(declaration.name, declaration.kind);
        }
        parser.take_token_if(TokenType::End)?;
        parser.exit_scope();

        Some(Self {
            scope: scope_id,
            kind,
        })
    }
}

#[derive(Debug, Clone)]
pub struct ExternFunction {
    pub symbol: String,
    pub parameters: Vec<Type>,
    pub return_type: Type,
}

impl Parse for ExternFunction {
    fn parse(parser: &mut Parser) -> Result<Self, Error> {
        parser.take_token_if(TokenType::At)?;
        if parser.parse::<Ident>()?.0 != "extern" {
            return None;
        };

        parser.take_token_if(TokenType::OpenParen)?;
        let Token::String(symbol) = parser.take_token()? else {
            return None;
        };

        parser.take_token_if(TokenType::Comma)?;
        parser.take_token_if(TokenType::Function)?;

        parser.take_token_if(TokenType::OpenParen)?;
        let parameters = parser.parse_csv();
        parser.take_token_if(TokenType::CloseParen)?;

        let return_type = parser.parse()?;
        let _ = parser.take_token_if(TokenType::Comma);

        parser.take_token_if(TokenType::CloseParen)?;

        Some(Self {
            symbol,
            parameters,
            return_type,
        })
    }
}

#[derive(Debug, Clone)]
pub enum DeclarationKind {
    Function(Function),
    ExternFunction(ExternFunction),
    Const(Expression),
    Union(Union),
    Struct(Struct),
    Primitive(Primitive),
}

#[derive(Debug, Clone)]
pub struct Declaration {
    pub name: Ident,
    pub kind: DeclarationKind,
}

impl Parse for Declaration {
    fn parse(parser: &mut Parser) -> Result<Self, Error> {
        let kind = parser.take_token()?;
        let name = parser.parse()?;
        parser.take_token_if(TokenType::Assignment)?;
        let kind = match kind {
            Token::Type => match parser.peek_token()? {
                Token::Union => DeclarationKind::Union(parser.parse()?),
                Token::Struct => DeclarationKind::Struct(parser.parse()?),
                Token::At => DeclarationKind::Primitive(parser.parse()?),
                _ => return None,
            },
            Token::Function => {
                if parser.peek_token()? == Token::At {
                    DeclarationKind::ExternFunction(parser.parse()?)
                } else {
                    DeclarationKind::Function(parser.parse()?)
                }
            }
            Token::Const => DeclarationKind::Const(parser.parse()?),
            _ => return None,
        };
        Some(Self { name, kind })
    }
}

#[test]
fn test_primitive() {
    let source = r#"@i32
        fn hi = () String
            "hello"
        end
    end"#;
    let primitive = Parser::from(Tokenizer::from(source))
        .parse::<Primitive>()
        .unwrap();
    let scope = primitive.scope;
    assert_eq!(
        primitive,
        Primitive {
            kind: PrimitiveKind::I32,
            scope
        }
    );
}

#[test]
fn top_level_decl_parses() {
    let source = r"const MONEY = INHERITANCE";
    assert_eq!(
        Parser::from(Tokenizer::from(source))
            .parse::<Declaration>()
            .unwrap(),
        Declaration {
            name: Ident(String::from("MONEY")),
            kind: DeclarationKind::Const(Expression::Ident(Ident(String::from("INHERITANCE"))))
        }
    );

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
    let source = r"(String a, b) UInt32
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
