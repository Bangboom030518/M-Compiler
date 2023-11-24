#![warn(clippy::pedantic, clippy::nursery)]

mod type_resolution;
use std::collections::HashMap;

use itertools::Itertools as _;
// use cranelift::prelude::*;
use parser::{
    expression::{IntrinsicCall, UnaryOperator},
    prelude::Literal,
    top_level::DeclarationKind,
    Expression,
};
use type_resolution::Type;

#[derive(Debug, Clone)]
enum Statement {
    Assignment(StackSlotId, Value),
    Ignore(Value),
    Return(Value),
}

#[derive(Debug, Clone)]
enum Value {
    U8Const(u8),
    U16Const(u16),
    U32Const(u32),
    U64Const(u64),
    U128Const(u128),
    I8Const(i8),
    I16Const(i16),
    I32Const(i32),
    I64Const(i64),
    I128Const(i128),
    F32Const(f32),
    F64Const(f64),
    UnknownSignedIntegerConst(i128),
    UnknownIntegerConst(u128),
    UnknownFloatConst(f64),
    IAdd(Box<Value>, Box<Value>),
    StackSlot(StackSlotId),
}

#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
#[error("Type error :)")]
// TODO: annotated results
enum SemanticError {
    InvalidIntegerLiteral,
    IntegerOverflow(#[from] std::num::TryFromIntError),
    UnknownType,
    InvalidAssignment,
    DeclarationNotFound,
}

#[derive(Default, Debug)]
struct LocalScope(Vec<Option<type_resolution::Id>>);

impl LocalScope {
    pub fn new() -> Self {
        Self::default()
    }

    // TODO: index operator
    pub fn get(&self, StackSlotId(index): StackSlotId) -> Option<type_resolution::Id> {
        *self.0.get(index).unwrap()
    }

    pub fn create_slot(&mut self, r#type: Option<type_resolution::Id>) -> StackSlotId {
        self.0.push(r#type);
        StackSlotId(self.0.len() - 1)
    }
}

#[derive(Clone, Copy, Debug)]
struct StackSlotId(usize);

#[derive(Debug)]
struct ExpressionContext<'a> {
    type_store: &'a type_resolution::TypeStore,
    return_type: Option<type_resolution::Id>,
    context_type: Option<type_resolution::Id>,
    names: HashMap<parser::Ident, StackSlotId>,
    local_scope: &'a mut LocalScope,
    scope_id: parser::scope::Id,
}

impl<'a> ExpressionContext<'a> {
    fn new(
        type_store: &'a type_resolution::TypeStore,
        local_scope: &'a mut LocalScope,
        scope_id: parser::scope::Id,
    ) -> Self {
        Self {
            type_store,
            return_type: None,
            context_type: None,
            local_scope,
            scope_id,
            names: HashMap::new(),
        }
    }

    fn return_type(self, r#type: type_resolution::Id) -> Self {
        Self {
            return_type: Some(r#type),
            ..self
        }
    }

    fn add_let_declaration(
        &mut self,
        identifier: &parser::Ident,
        expression: &Expression,
    ) -> Result<Statement, SemanticError> {
        let (value, r#type) = self.get_expression_information(&expression)?;
        let stack_slot = self.local_scope.create_slot(r#type);
        self.names.insert(identifier.clone(), stack_slot);
        if r#type != self.local_scope.get(stack_slot) {
            return Err(SemanticError::InvalidAssignment);
        };
        Ok(Statement::Assignment(stack_slot, value))
    }

    fn handle_statement(
        &mut self,
        statement: &parser::Statement,
    ) -> Result<Statement, SemanticError> {
        match statement {
            parser::Statement::Assignment(parser::Assignment(name, expression)) => {
                let stack_slot = *self.names.get(name).unwrap_or_else(|| todo!());
                let (value, r#type) = self.get_expression_information(expression)?;
                if r#type != self.local_scope.get(stack_slot) {
                    return Err(SemanticError::InvalidAssignment);
                };
                Ok(Statement::Assignment(stack_slot, value))
            }
            parser::Statement::Let(ident, expression) => {
                self.add_let_declaration(ident, expression)
            }
            parser::Statement::Expression(expression) => {
                if let Expression::Return(expression) = expression {
                    self.context_type = self.return_type;
                    Ok(Statement::Return(
                        self.get_expression_information(&expression.clone())?.0,
                    ))
                } else {
                    Ok(Statement::Ignore(
                        self.get_expression_information(expression)?.0,
                    ))
                }
            }
        }
    }

    fn get_expression_information(
        &mut self,
        expression: &Expression,
    ) -> Result<(Value, Option<type_resolution::Id>), SemanticError> {
        let type_id = self.context_type;
        // .map_or_else(|| Err(SemanticError::UnknownType), Ok)?;
        let r#type = type_id.map(|type_id| self.type_store.get(type_id));

        match expression {
            Expression::Literal(literal) => match literal {
                Literal::Integer(integer) => {
                    let value = match r#type {
                        Some(Type::U8) => Value::U8Const(u8::try_from(*integer)?),
                        Some(Type::U16) => Value::U16Const(u16::try_from(*integer)?),
                        Some(Type::U32) => Value::U32Const(u32::try_from(*integer)?),
                        Some(Type::U64) => Value::U64Const(u64::try_from(*integer)?),
                        Some(Type::U128) => Value::U128Const(*integer),
                        Some(Type::I8) => Value::I8Const(i8::try_from(*integer)?),
                        Some(Type::I16) => Value::I16Const(i16::try_from(*integer)?),
                        Some(Type::I32) => Value::I32Const(i32::try_from(*integer)?),
                        Some(Type::I64) => Value::I64Const(i64::try_from(*integer)?),
                        Some(Type::I128) => Value::I128Const(i128::try_from(*integer)?),
                        None => Value::UnknownIntegerConst(*integer),
                        Some(_) => return Err(SemanticError::InvalidIntegerLiteral),
                    };

                    Ok((value, type_id))
                }
                Literal::Float(float) => {
                    let value = match r#type {
                        Some(Type::F32) => Value::F32Const(*float as f32),
                        Some(Type::F64) => Value::F64Const(*float),
                        None => Value::UnknownFloatConst(*float),
                        Some(_) => return Err(SemanticError::InvalidIntegerLiteral),
                    };
                    Ok((value, type_id))
                }
                _ => todo!(),
            },
            Expression::UnaryPrefix(operator, expression) => match (*operator, expression.as_ref())
            {
                (UnaryOperator::Minus, Expression::Literal(Literal::Integer(integer))) => {
                    let value = match r#type {
                        Some(Type::I8) => Value::I8Const(-i8::try_from(*integer)?),
                        Some(Type::I16) => Value::I16Const(-i16::try_from(*integer)?),
                        Some(Type::I32) => Value::I32Const(-i32::try_from(*integer)?),
                        Some(Type::I64) => Value::I64Const(-i64::try_from(*integer)?),
                        Some(Type::I128) => Value::I128Const(-i128::try_from(*integer)?),
                        None => Value::UnknownSignedIntegerConst(-i128::try_from(*integer)?),
                        Some(_) => return Err(SemanticError::InvalidIntegerLiteral),
                    };
                    Ok((value, type_id))
                }
                (UnaryOperator::Minus, Expression::Literal(Literal::Float(float))) => {
                    let value = match r#type {
                        Some(Type::F32) => Value::F32Const(-(*float as f32)),
                        Some(Type::F64) => Value::F64Const(-*float),
                        None => Value::UnknownFloatConst(-*float),
                        Some(_) => return Err(SemanticError::InvalidIntegerLiteral),
                    };
                    Ok((value, type_id))
                }
                _ => todo!(),
            },
            Expression::IntrinsicCall(intrinsic) => match intrinsic {
                IntrinsicCall::IAdd(left, right) => {
                    // TODO: infer left type from right
                    let left = self.get_expression_information(left)?;
                    self.context_type = left.1;
                    let right = self.get_expression_information(right)?;
                    Ok((
                        Value::IAdd(Box::new(left.0), Box::new(right.0)),
                        left.1.or(right.1),
                    ))
                }
                IntrinsicCall::AssertType(expression, r#type) => {
                    let r#type = self
                        .type_store
                        .lookup(
                            match r#type {
                                parser::Type::Identifier(identifier) => identifier,
                            },
                            self.scope_id,
                        )
                        .map_or_else(|| Err(SemanticError::DeclarationNotFound), Ok)?;
                    self.context_type = Some(r#type);
                    let expression = self.get_expression_information(expression)?.0;
                    Ok((expression, Some(r#type)))
                }
            },
            Expression::Return(expression) => {
                self.context_type = self.return_type;
                self.get_expression_information(expression)
            }
            Expression::Identifier(ident) => {
                let stack_slot = *self
                    .names
                    .get(ident)
                    .map_or_else(|| Err(SemanticError::DeclarationNotFound), Ok)?;
                let type_id = self.local_scope.get(stack_slot);
                Ok((Value::StackSlot(stack_slot), type_id))
            }
            _ => todo!(),
        }
    }
}

fn main() {
    let file = parser::parse_file(include_str!("../../input.m")).unwrap();
    let root = file.root;
    let mut type_store = type_resolution::TypeStore {
        types: Vec::new(),
        scopes: HashMap::new(),
        file_cache: file.cache,
    };

    type_resolution::TypeScope::append_new(&mut type_store, file.root).unwrap();
    for declaration in type_store.file_cache[root].declarations.values() {
        let DeclarationKind::Function(function) = declaration else {
            continue;
        };

        let Some((return_statement, statements)) = function.body.split_last() else {
            todo!("handle empty body")
        };

        // TODO: `.clone()`?
        let return_statement = if let parser::Statement::Expression(expression) = return_statement {
            parser::Statement::Expression(Expression::Return(Box::new(expression.clone())))
        } else {
            return_statement.clone()
        };

        let mut local_scope = LocalScope::new();
        let context = ExpressionContext::new(&type_store, &mut local_scope, root);
        // TODO: not root
        let mut context = context.return_type(
            type_store
                .lookup(
                    match function.return_type.as_ref().unwrap() {
                        parser::Type::Identifier(ident) => ident,
                    },
                    root,
                )
                .unwrap(),
        );
        let statements = statements
            .iter()
            .chain(std::iter::once(&return_statement))
            .map(|statement| context.handle_statement(statement))
            .collect::<Result<Vec<_>, _>>()
            .unwrap();

        dbg!(&statements);
        dbg!(&context);
        dbg!(&type_store.types);
    }
}
