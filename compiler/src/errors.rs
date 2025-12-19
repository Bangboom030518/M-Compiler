use crate::declarations::Reference;
use ariadne::{Color, ColorGenerator, Fmt, Label, Report, ReportKind, Source};
use tokenizer::Span;

// struct ContextualSpan {
//     row: usize,
//     column: usize,
// }

// impl ContextualSpan {
//     fn new(span: Span, input: &str) -> Self {}
// }

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Error {
    pub kind: Kind,
    pub span: Span,
}

impl Error {
    #[must_use]
    pub const fn new(kind: Kind, span: Span) -> Self {
        Self { kind, span }
    }

    #[must_use]
    pub fn print(self, input: &str, filename: &str) {
        Report::build(ReportKind::Error, (filename, self.span.clone()))
            .with_code(67)
            .with_message("Oop noob alert!".to_string())
            .with_label(Label::new((filename, self.span)).with_message(self.kind.message()))
            .with_note("You're a bit of a silly billy :)".to_string())
            .finish()
            .print(("input.m", Source::from(input)))
            .unwrap();
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

impl Kind {
    pub fn message(&self) -> String {
        format!("{self:?}")
    }
}
