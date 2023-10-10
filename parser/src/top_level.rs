use crate::internal::prelude::*;

#[derive(PartialEq, Eq, Debug)]
// TODO: rename
pub struct TypeBinding {
    pub r#type: Option<Type>,
    pub name: Identifier,
}

impl TypeBinding {
    fn parse(parser: &mut Parser, peek: impl Fn(&mut Parser) -> bool) -> Option<Self> {
        let r#type = parser.parse()?;
        if peek(parser) {
            Some(Self {
                name: match r#type {
                    Type::Identifier(ident) => ident,
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

#[derive(PartialEq, Eq, Debug)]
pub struct Variant(TypeBinding);

impl Parse for Variant {
    fn parse(parser: &mut Parser) -> Option<Self> {
        TypeBinding::parse(parser, |parser| parser.peek_newline_or_eof().is_some()).map(Self)
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct Field {
    pub r#type: Type,
    pub name: Identifier,
}

impl Parse for Field {
    fn parse(parser: &mut Parser) -> Option<Self> {
        Some(Self {
            r#type: parser.parse()?,
            name: parser.parse()?,
        })
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct Parameter(TypeBinding);

impl Parse for Parameter {
    fn parse(parser: &mut Parser) -> Option<Self> {
        TypeBinding::parse(parser, |parser| {
            parser.peek_token_if(&Token::Comma).is_some()
        })
        .map(Self)
    }
}

#[derive(PartialEq, Debug)]
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
        let scope = parser.get_scope(scope_id);

        let mut fields = Vec::new();
        // TODO: refactor to iter
        while let Some(input) = parser.parse_line() {
            fields.push(input);
        }

        while let Some(declaration) = parser.parse_line::<Declaration>() {
            scope
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

#[derive(PartialEq, Debug)]
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
        let scope = parser.get_scope(scope_id);

        let mut variants = Vec::new();
        // TODO: refactor to iter
        while let Some(input) = parser.parse_line() {
            variants.push(input);
        }

        while let Some(declaration) = parser.parse_line::<Declaration>() {
            scope
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

#[derive(PartialEq, Debug)]
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

#[derive(PartialEq, Debug)]
pub enum DeclarationKind {
    Function(Function),
    Const(Expression),
    Union(Union),
    Struct(Struct),
}

#[derive(PartialEq, Debug)]
pub struct Declaration {
    pub name: Identifier,
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
fn top_level_decl_parses() {
    let source = r"const MONEY = INHERITANCE";
    assert_eq!(
        Parser::from(Tokenizer::from(source))
            .parse::<Declaration>()
            .unwrap(),
        Declaration {
            name: Identifier(String::from("MONEY")),
            kind: DeclarationKind::Const(Expression::Identifier(Identifier(String::from(
                "INHERITANCE"
            ))))
        }
    );
    let uint_8 = Type::Identifier(Identifier(String::from("UInt8")));
    let source = r"type Point = struct
    UInt8 x
    UInt8 y";
    assert_eq!(
        Parser::new(Tokenizer::from(source), &mut scope::Cache::new())
            .parse::<Declaration>()
            .unwrap(),
        Declaration {
            name: Identifier(String::from("Point")),
            kind: DeclarationKind::Struct(Struct {
                fields: vec![
                    Field {
                        r#type: uint_8.clone(),
                        name: Identifier(String::from("x"))
                    },
                    Field {
                        r#type: uint_8,
                        name: Identifier(String::from("y"))
                    }
                ],
            })
        }
    );
}

#[test]
fn union_parses() {
    let source = r"union
    String a
    b";
    assert_eq!(
        Parser::new(Tokenizer::from(source), &mut scope::Cache::new())
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
    let source = r"(String a, b,) UInt32
    a
    a";
    assert_matches!(
        Parser::from(Tokenizer::from(source))
            .parse::<Function>()
            .unwrap(),
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
            return_type: Some(Type::Identifier(Identifier(String::from("UInt32")))),
            body: vec![
                Statement::Expression(Expression::Identifier(Identifier(String::from("a")))),
                Statement::Expression(Expression::Identifier(Identifier(String::from("a"))))
            ],
            scope: _
        }
    );
}

pub mod prelude {
    pub use super::{Function, Struct, Union};
}
