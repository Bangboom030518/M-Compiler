use crate::declarations::Reference;
use tokenizer::Span;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Error {
    pub span: Span,
    pub kind: Kind,
}

impl Error {
    #[must_use]
    pub fn message(self, input: &str) -> String {
        let start = self.span.start.saturating_sub(5);
        let end = (self.span.end + 5).min(input.len());
        let range = input.get(start..end).expect("span should be in range");
        format!("Error in '{range}': {:?}", self.kind)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DeclarationConstraint {
    Length,
    Function,
    Type,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TypeConstraint {
    String,
    Integer,
    Float,
    Address,
    StorableOrLoadable,
    Bool,
    Array,
    Struct,
    Number,
    NotVoid,
}

#[derive(Clone, Debug, PartialEq, Eq)]
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
