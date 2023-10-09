#![warn(clippy::pedantic, clippy::nursery)]

use ::parser::prelude::*;
use std::collections::HashMap;
use itertools::Itertools;

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
struct ScopeId(usize);

#[derive(Debug)]
pub enum Expression {
    Integer(u64),
    Float(f64),
    String(String),
    // List,
    Identifier(Identifier),
    Call(Box<Expression>, Vec<::parser::Type>, Vec<Expression>),
}

impl From<::parser::Expression> for Expression {
    fn from(value: ::parser::Expression) -> Self {
        use ::parser::Expression;
        match value {
            Expression::Identifier(identifier) => Self::Identifier(identifier),
            Expression::Literal(literal) => match literal {
                Literal::Float(float) => Self::Float(float),
                Literal::Integer(integer) => Self::Integer(integer),
                Literal::String(string) => Self::String(string),
            },
            Expression::Call(expression::Call {
                arguments,
                callable,
                type_arguments,
            }) => Self::Call(
                Box::new(Self::from(*callable)),
                type_arguments,
                arguments.into_iter().map(Into::into).collect_vec(),
            ),
            // @add(1, 2)
            Expression::Binary(binary) => todo!("binary expressions")
        }
    }
}

#[derive(Debug)]
enum Declaration {
    Struct(Struct),
    // Union,
    Function(Function),
    Expression(Expression),
    // Const,
    // Let(),
}

impl Declaration {
    pub fn new(
        declaration: top_level::Declaration,
        scope_id: ScopeId,
        scope_cache: &mut ScopeCache,
    ) -> Self {
        match declaration.kind {
            top_level::DeclarationKind::Struct(r#struct) => {
                Self::Struct(Struct::new(r#struct, scope_id, scope_cache))
            }
            kind => todo!("implement {kind:?}"),
        }
    }
}

#[derive(Debug)]
struct Function {
    scope: ScopeId,
    statements: Vec<Statement>,
}

#[derive(Debug, Default)]
struct ScopeCache(Vec<Scope>);

impl ScopeCache {
    fn new() -> Self {
        Self::default()
    }

    fn create_root_scope(
        &mut self,
        init: impl FnOnce(ScopeId, &mut Self) -> HashMap<Identifier, Declaration>,
    ) -> ScopeId {
        let id = ScopeId(self.0.len());
        let declarations = init(id, self);
        self.0.push(Scope {
            parent: None,
            declarations,
        });
        id
    }

    fn create_scope(
        &mut self,
        parent: ScopeId,
        init: impl FnOnce(ScopeId, &mut Self) -> HashMap<Identifier, Declaration>,
    ) -> ScopeId {
        let id = ScopeId(self.0.len());
        let declarations = init(id, self);
        self.0.push(Scope {
            parent: Some(parent),
            declarations,
        });
        id
    }

    // TODO: index operator instead?
    fn get(&mut self, ScopeId(id): ScopeId) -> &mut Scope {
        &mut self.0[id]
    }
}

#[derive(Debug)]
struct Scope {
    parent: Option<ScopeId>,
    declarations: HashMap<Identifier, Declaration>,
}

#[derive(Debug)]
struct Struct {
    fields: Vec<(Identifier, ::parser::Type)>,
    scope: ScopeId,
}

impl Struct {
    fn new(
        top_level::Struct {
            fields,
            declarations,
        }: top_level::Struct,
        scope_id: ScopeId,
        scope_cache: &mut ScopeCache,
    ) -> Self {
        let scope = scope_cache.create_scope(scope_id, |id, scope_cache| {
            declarations
                .into_iter()
                .map(|declaration| {
                    (
                        declaration.name.clone(),
                        Declaration::new(declaration, id, scope_cache),
                    )
                })
                .collect()
        });

        Self {
            fields: fields
                .into_iter()
                .map(|field| (field.name, field.r#type))
                .collect(),
            scope,
        }
    }
}

#[derive(Debug)]
struct Union {
    variants: Vec<(Identifier, ::parser::Type)>,
    scope: ScopeId,
}

fn main() {
    let ast = parse_file(include_str!("../../input.m")).unwrap();
    dbg!(&ast);
    let mut scope_cache = ScopeCache::new();
    scope_cache.create_root_scope(|scope_id, scope_cache| {
        ast.into_iter()
            .map(|declaration| {
                (
                    declaration.name.clone(),
                    Declaration::new(declaration, scope_id, scope_cache),
                )
            })
            .collect()
    });
    dbg!(scope_cache);
}
