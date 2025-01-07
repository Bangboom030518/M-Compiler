use crate::declarations::{self, Declarations, Reference};
pub use builder::Builder;
use builder::VariableId;
use parser::expression::IntrinsicOperator;

pub mod builder;

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub expression: Option<Typed<Expression>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assignment {
    pub left: Typed<Expression>,
    pub right: Typed<Expression>,
}

impl Assignment {
    pub const fn new(left: Typed<Expression>, right: Typed<Expression>) -> Self {
        Self { left, right }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Expression(Typed<Expression>),
    Assignment(Assignment),
    Let(VariableId, Typed<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryIntrinsic {
    pub left: Typed<Expression>,
    pub right: Typed<Expression>,
    pub operator: IntrinsicOperator,
}

#[derive(Debug, Clone, PartialEq)]
pub struct If {
    pub condition: Typed<Expression>,
    pub then_branch: Block,
    pub else_branch: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Call {
    pub callable: Typed<Expression>,
    pub arguments: Vec<Typed<Expression>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Constructor(pub Vec<(String, Typed<Expression>)>);

#[derive(Debug, Clone, PartialEq)]
pub struct FieldAccess {
    pub expression: Typed<Expression>,
    pub field: parser::Ident,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Store {
    pub pointer: Typed<Expression>,
    pub expression: Typed<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Generixed {
    pub expression: Typed<Expression>,
    pub generics: Vec<Reference>,
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
    Addr(Box<Typed<Expression>>),
    Call(Box<Call>),
    Return(Box<Typed<Expression>>),
    Load(Box<Typed<Expression>>),
    Store(Box<Store>),
    LocalAccess(VariableId),
    GlobalAccess(declarations::Id),
    Generixed(Box<Generixed>),
    AssertType(Box<Typed<Expression>>),
}

impl Expression {
    pub fn typed(self, declarations: &mut Declarations) -> Typed<Self> {
        Typed::new(self, declarations.create_type_ref())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Typed<T> {
    pub value: T,
    pub type_ref: Reference,
}

impl<T> Typed<T> {
    pub const fn new(value: T, type_ref: Reference) -> Self {
        Self { value, type_ref }
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Typed<U> {
        Typed {
            value: f(self.value),
            type_ref: self.type_ref,
        }
    }

    pub fn boxed(self) -> Typed<Box<T>> {
        self.map(Box::new)
    }
}
