use std::collections::HashMap;

use crate::declarations::{self, Declarations};
use crate::layout::Layout;
use crate::SemanticError;
pub use builder::Builder;
use builder::VariableId;
use cranelift::codegen::ir::immediates::Offset32;
use cranelift::prelude::*;
use parser::expression::{CmpOperator, IntrinsicOperator};

pub mod builder;

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(TypedExpression),
    Assignment(TypedExpression, TypedExpression),
    Let(Variable, TypedExpression),
}

impl Statement {
    fn infer(
        &mut self,
        variables: &mut HashMap<VariableId, Option<declarations::Id>>,
        return_type: declarations::Id,
        declarations: &Declarations,
    ) -> Result<bool, SemanticError> {
        match self {
            Self::Assignment(
                TypedExpression {
                    expression: Expression::LocalAccess(var),
                    ..
                },
                expression,
            ) => todo!(),
            Self::Let(_, _) => todo!(),
            Self::Expression(expression) => expression.infer(variables, return_type, declarations),
            Self::Assignment(_, _) => todo!(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct BinaryIntrinsic {
    pub left: TypedExpression,
    pub right: TypedExpression,
    pub operator: IntrinsicOperator,
}

#[derive(Debug, Clone)]
pub struct If {
    pub condition: TypedExpression,
    pub then_branch: Vec<Statement>,
    pub else_branch: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub callable: TypedExpression,
    pub arguments: Vec<TypedExpression>,
}

#[derive(Debug, Clone)]
pub struct Constructor(pub Vec<(Offset32, TypedExpression)>);

#[derive(Debug, Clone)]
pub enum Expression {
    IntegerConst(u128),
    FloatConst(f64),
    BinaryIntrinsic(Box<BinaryIntrinsic>),
    If(Box<If>),
    FieldAccess(Box<TypedExpression>, parser::Ident),
    Constructor(Constructor),
    MutablePointer(Box<TypedExpression>),
    Call(Box<Call>),
    Return(Box<TypedExpression>),
    LocalAccess(VariableId),
    GlobalAccess(declarations::Id),
}

impl Expression {
    pub fn infer(
        &mut self,
        variables: &mut HashMap<VariableId, Option<declarations::Id>>,
        return_type: declarations::Id,
        declarations: &Declarations,
    ) -> Result<(Option<declarations::Id>, bool), SemanticError> {
        let result = match self {
            Self::FloatConst(_) | Self::IntegerConst(_) => (None, false),
            Self::LocalAccess(variable) => (
                *variables
                    .get(variable)
                    .expect("Variable doesn't exist in hir!"),
                false,
            ),
            Self::FieldAccess(expression, field) => {
                let has_mutated = expression.infer(variables, return_type, declarations)?;
                let type_id = expression.type_id;
                match type_id {
                    None => (None, has_mutated),
                    Some(type_id) => {
                        let Layout::Struct { fields, .. } = declarations.get_layout(type_id) else {
                            return Err(SemanticError::NonStructFieldAccess);
                        };
                        let field = fields.get(field).ok_or(SemanticError::NonExistentField)?;
                        (Some(field.type_id), has_mutated)
                    }
                }
            }
            Self::Return(expression) => {
                if let Some(type_id) = expression.type_id {
                    if type_id != return_type {
                        return Err(SemanticError::MismatchedTypes);
                    }
                }
                expression.type_id = Some(return_type);
                let has_mutated = expression.infer(variables, return_type, declarations)?;
                (None, has_mutated)
            }
            _ => (None, false),
        };

        Ok(result)
    }
}

#[derive(Debug, Clone)]
pub struct TypedExpression {
    pub expression: Expression,
    pub type_id: Option<declarations::Id>,
}

impl TypedExpression {
    pub fn infer(
        &mut self,
        variables: &mut HashMap<VariableId, Option<declarations::Id>>,
        return_type: declarations::Id,
        declarations: &Declarations,
    ) -> Result<bool, SemanticError> {
        if self.type_id.is_some() {
            return Ok(false);
        }

        let (type_id, has_mutated_environment) =
            self.expression
                .infer(variables, return_type, declarations)?;
        self.type_id = type_id;
        Ok(has_mutated_environment)
    }

    pub fn new(expression: Expression, type_id: Option<declarations::Id>) -> Self {
        Self {
            expression,
            type_id,
        }
    }

    pub fn with_type(self, type_id: declarations::Id) -> Self {
        Self {
            type_id: Some(type_id),
            ..self
        }
    }
}

impl From<Expression> for TypedExpression {
    fn from(expression: Expression) -> Self {
        Self {
            expression,
            type_id: None,
        }
    }
}
