use crate::declarations;
pub use builder::Builder;
use builder::VariableId;
use cranelift::codegen::ir::immediates::Offset32;
use cranelift::prelude::*;
use parser::expression::IntrinsicOperator;

pub mod builder;
pub mod inferer;

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(TypedExpression),
    Assignment(TypedExpression, TypedExpression),
    Let(Variable, TypedExpression),
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

#[derive(Debug, Clone)]
pub struct TypedExpression {
    pub expression: Expression,
    pub type_id: Option<declarations::Id>,
}

impl TypedExpression {
    pub const fn new(expression: Expression, type_id: Option<declarations::Id>) -> Self {
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
