use crate::internal::prelude::*;

#[derive(PartialEq, Eq, Debug, Clone)]
// TODO: rename
pub struct TypeBinding {
    pub r#type: Option<Type>,
    pub name: Ident,
}

impl TypeBinding {
    fn parse(parser: &mut Parser, peek: impl Fn(&mut Parser) -> bool) -> Option<Self> {
        let r#type = parser.parse()?;
        if peek(parser) {
            Some(Self {
                name: match r#type {
                    Type::Identifier(ident) => ident,
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
    fn parse(parser: &mut Parser) -> Option<Self> {
        TypeBinding::parse(parser, |parser| parser.peek_newline_or_eof().is_some()).map(Self)
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Field {
    pub r#type: Type,
    pub name: Ident,
}

impl Parse for Field {
    fn parse(parser: &mut Parser) -> Option<Self> {
        Some(Self {
            r#type: parser.parse()?,
            name: parser.parse()?,
        })
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Parameter(TypeBinding);

impl Parse for Parameter {
    fn parse(parser: &mut Parser) -> Option<Self> {
        TypeBinding::parse(parser, |parser| {
            parser.peek_token_if(&Token::Comma).is_some()
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
    fn parse(parser: &mut Parser) -> Option<Self> {
        parser.take_token_if(&Token::Struct)?;
        parser.take_newline()?;
        parser.indent();
        let scope_id = parser.create_scope();

        let mut fields = Vec::new();
        // TODO: refactor to iter
        while let Some(input) = parser.parse_line() {
            fields.push(input);
        }

        while let Some(declaration) = parser.parse_line::<Declaration>() {
            parser
                .get_scope(scope_id)
                .declarations
                .insert(declaration.name, declaration.kind);
        }
        parser.unindent();
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
    fn parse(parser: &mut Parser) -> Option<Self> {
        parser.take_token_if(&Token::Union)?;
        parser.take_newline()?;
        parser.indent();
        let scope_id = parser.create_scope();
        let mut variants = Vec::new();
        // TODO: refactor to iter
        while let Some(input) = parser.parse_line() {
            variants.push(input);
        }

        while let Some(declaration) = parser.parse_line::<Declaration>() {
            parser
                .get_scope(scope_id)
                .declarations
                .insert(declaration.name, declaration.kind);
        }
        parser.unindent();
        parser.exit_scope();

        Some(Self {
            variants,
            scope: scope_id,
        })
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Function {
    pub parameters: Vec<Parameter>,
    pub return_type: Option<Type>,
    pub body: Vec<Statement>,
}

impl Parse for Function {
    fn parse(parser: &mut Parser) -> Option<Self> {
        parser.take_token_if(&Token::OpenParen);

        let parameters = parser.parse_csv();

        parser.take_token_if(&Token::CloseParen);

        let return_type = parser.parse();
        let mut body = Vec::new();
        parser.take_newline()?;

        parser.indent();
        while let Some(input) = parser.parse_line() {
            body.push(input);
        }
        parser.unindent();

        Some(Self {
            parameters,
            return_type,
            body,
        })
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Primitive {
    pub kind: PrimitiveKind,
    pub scope: scope::Id,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
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
}

impl Parse for PrimitiveKind {
    fn parse(parser: &mut Parser) -> Option<Self> {
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
            _ => return None,
        };
        Some(kind)
    }
}

impl Parse for Primitive {
    fn parse(parser: &mut Parser) -> Option<Self> {
        parser.take_token_if(&Token::At)?;
        let kind = parser.parse()?;
        parser.take_newline()?;
        parser.indent();
        let scope_id = parser.create_scope();
        while let Some(declaration) = parser.parse_line::<Declaration>() {
            parser
                .get_scope(scope_id)
                .declarations
                .insert(declaration.name, declaration.kind);
        }
        parser.unindent();
        parser.exit_scope();

        Some(Self {
            scope: scope_id,
            kind,
        })
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum DeclarationKind {
    Function(Function),
    Const(Expression),
    Union(Union),
    Struct(Struct),
    Primitive(Primitive),
}

#[derive(PartialEq, Debug, Clone)]
pub struct Declaration {
    pub name: Ident,
    pub kind: DeclarationKind,
}

impl Parse for Declaration {
    fn parse(parser: &mut Parser) -> Option<Self> {
        let kind = parser.take_token()?;
        let name = parser.parse()?;
        parser.take_token_if(&Token::Assignment)?;
        let kind = match kind {
            Token::Type => match parser.peek_token()? {
                Token::Union => DeclarationKind::Union(parser.parse()?),
                Token::Struct => DeclarationKind::Struct(parser.parse()?),
                Token::At => DeclarationKind::Primitive(parser.parse()?),
                _ => return None,
            },
            Token::Function => DeclarationKind::Function(parser.parse()?),
            Token::Const => DeclarationKind::Const(parser.parse()?),
            _ => return None,
        };
        Some(Self { name, kind })
    }
}

#[test]
fn test_primitive() {
    let source = r#"@i32
        function hi = () ->
            "hello""#;
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
            kind: DeclarationKind::Const(Expression::Identifier(Ident(String::from(
                "INHERITANCE"
            ))))
        }
    );

    let uint_8 = Type::Identifier(Ident(String::from("UInt8")));
    let source = r"type Point = struct
    UInt8 x
    UInt8 y";
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
    b";
    let union = Parser::from(Tokenizer::from(source))
        .parse::<Union>()
        .unwrap();
    let scope = union.scope;
    assert_eq!(
        union,
        Union {
            variants: vec![
                Variant(TypeBinding {
                    r#type: Some(Type::Identifier(Ident(String::from("String")))),
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
    let source = r"(String a, b,) UInt32
    a
    a";
    assert_eq!(
        Parser::from(Tokenizer::from(source))
            .parse::<Function>()
            .unwrap(),
        Function {
            parameters: vec![
                Parameter(TypeBinding {
                    r#type: Some(Type::Identifier(Ident(String::from("String")))),
                    name: Ident(String::from("a")),
                }),
                Parameter(TypeBinding {
                    r#type: None,
                    name: Ident(String::from("b")),
                }),
            ],
            return_type: Some(Type::Identifier(Ident(String::from("UInt32")))),
            body: vec![
                Statement::Expression(Expression::Identifier(Ident(String::from("a")))),
                Statement::Expression(Expression::Identifier(Ident(String::from("a"))))
            ],
        }
    );
}

pub mod prelude {
    pub use super::{Function, Struct, Union};
}
