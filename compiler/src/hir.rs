use crate::declarations::{self, Declarations};
use crate::layout::Layout;
use crate::SemanticError;
pub use builder::Builder;
use cranelift::codegen::ir::immediates::Offset32;
use cranelift::prelude::*;
use parser::expression::IntrinsicOperator;

use self::builder::VariableId;

mod builder;

#[derive(Debug, Clone)]
pub enum Statement {
    Ignore(TypedExpression),
    Assignment(TypedExpression, TypedExpression),
    Let(Variable, TypedExpression),
}

impl Statement {
    fn infer(
        &mut self,
        function: &mut builder::Function,
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
            Self::Ignore(expression) => expression.infer(function, declarations),
            _ => todo!(),
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
        function: &mut builder::Function,
        declarations: &Declarations,
    ) -> Result<(Option<declarations::Id>, bool), SemanticError> {
        let result = match self {
            Self::FloatConst(_) | Self::IntegerConst(_) => (None, false),
            Self::LocalAccess(variable) => (
                *function
                    .variables
                    .get(&variable)
                    .expect("Variable doesn't exist in hir!"),
                false,
            ),
            Self::FieldAccess(expression, field) => {
                let has_mutated = expression.infer(function, declarations)?;
                let type_id = expression.type_id;
                match type_id {
                    None => (None, has_mutated),
                    Some(type_id) => {
                        let Layout::Struct { fields, .. } = declarations.get_layout(type_id) else {
                            return Err(SemanticError::NonStructFieldAccess);
                        };
                        let field = fields.get(&field).ok_or(SemanticError::NonExistentField)?;
                        (Some(field.type_id), has_mutated)
                    }
                }
            }
            _ => todo!(),
        };

        Ok(result)
    }
}

#[derive(Debug, Clone)]
pub struct TypedExpression {
    expression: Expression,
    type_id: Option<declarations::Id>,
}

impl TypedExpression {
    pub fn infer(
        &mut self,
        function: &mut builder::Function,
        declarations: &Declarations,
    ) -> Result<bool, SemanticError> {
        if self.type_id.is_some() {
            return Ok(false);
        }

        let (type_id, has_mutated_environment) = self.expression.infer(function, declarations)?;
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
