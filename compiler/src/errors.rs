use tokenizer::Span;

use crate::declarations::Reference;

#[derive(Clone, Debug, PartialEq)]
pub struct Error {
    pub span: Span,
    pub kind: Kind,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum DeclarationConstraint {
    Length,
    Function,
    Type,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TypeConstraint {
    String,
    Integer,
    Float,
    Address,
    Bool,
    Array,
    Struct,
    Number,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Kind {
    DeclarationNotFound {
        ident: String,
    },
    FieldNotFound {
        parent_struct: Reference,
        field: String,
    },
    MismatchedTypes {
        expected: Reference,
        found: Reference,
    },
    MismatchedLengths {
        expected: u32,
        found: u32,
    },
    MismatchedArguments {
        arguments: usize,
        parameters: usize,
        declaration: Span,
    },
    MismatchedGenericArguments {
        arguments: usize,
        parameters: usize,
        declaration: Span,
    },
    CannotInferType,
    MissingParameterType,
    MissingReturnType,
    LengthTooBig,
    StructTooBig,
    IntegerLiteralTooBig,
    TypeConstraintViolation {
        constraint: TypeConstraint,
        found: Reference,
    },
    DeclarationConstraintViolation {
        constraint: DeclarationConstraint,
        found: Reference,
    },
    MissingStructFields {
        parent_struct: Reference,
        fields: Vec<String>,
    },
}
