use crate::declarations::{self, TypeReference};
use crate::SemanticError;
pub use builder::Builder;
use builder::VariableId;
use cranelift::codegen::ir::immediates::Offset32;
use parser::expression::IntrinsicOperator;

pub mod builder;
pub mod inferer;

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub expression: Option<TypedExpression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assignment {
    pub left: TypedExpression,
    pub right: TypedExpression,
}

impl Assignment {
    pub const fn new(left: TypedExpression, right: TypedExpression) -> Self {
        Self { left, right }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Expression(TypedExpression),
    Assignment(Assignment),
    Let(VariableId, TypedExpression),
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryIntrinsic {
    pub left: TypedExpression,
    pub right: TypedExpression,
    pub operator: IntrinsicOperator,
}

#[derive(Debug, Clone, PartialEq)]
pub struct If {
    pub condition: TypedExpression,
    pub then_branch: Block,
    pub else_branch: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Call {
    pub callable: TypedExpression,
    pub arguments: Vec<TypedExpression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Constructor(pub Vec<(Offset32, TypedExpression)>);

#[derive(Debug, Clone, PartialEq)]
pub struct FieldAccess {
    pub expression: TypedExpression,
    pub field: parser::Ident,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Store {
    pub pointer: TypedExpression,
    pub expression: TypedExpression,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    IntegerConst(u128),
    FloatConst(f64),
    StringConst(String),
    BinaryIntrinsic(Box<BinaryIntrinsic>),
    If(Box<If>),
    FieldAccess(Box<FieldAccess>),
    Constructor(Constructor),
    Addr(Box<TypedExpression>),
    Call(Box<Call>),
    Return(Box<TypedExpression>),
    Load(Box<TypedExpression>),
    Store(Box<Store>),
    LocalAccess(VariableId),
    GlobalAccess(declarations::Id),
}

impl Expression {
    pub fn with_type(self, type_ref: TypeReference) -> TypedExpression {
        TypedExpression {
            expression: self,
            type_ref: Some(type_ref),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedExpression {
    pub expression: Expression,
    pub type_ref: Option<TypeReference>,
}

impl TypedExpression {
    pub fn with_type(self, type_ref: TypeReference) -> Self {
        Self {
            type_ref: Some(type_ref),
            ..self
        }
    }

    pub fn expect_type(&self) -> Result<&TypeReference, SemanticError> {
        self.type_ref
            .as_ref()
            .ok_or_else(|| SemanticError::UnknownType(self.expression.clone()))
    }
}

impl From<Expression> for TypedExpression {
    fn from(expression: Expression) -> Self {
        Self {
            expression,
            type_ref: None,
        }
    }
}
