use crate::declarations::SpannedReference;
use ariadne::{Label, Report, ReportKind, Source};
use tokenizer::Span;

#[derive(Clone, Debug)]
pub struct Error {
    pub kind: Kind,
    pub span: Span,
}

impl Error {
    #[must_use]
    pub const fn new(kind: Kind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn print(self, input: &str, filename: &str) {
        Report::build(ReportKind::Error, (filename, 0..input.len()))
            .with_message(self.kind.message())
            .with_labels(self.kind.labels(filename, self.span))
            .finish()
            .print(("input.m", Source::from(input)))
            .expect("malformed error report");
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

#[derive(Clone, Debug)]
pub enum Kind {
    DeclarationNotFound {
        ident: String,
    },
    FieldNotFound {
        parent_struct: SpannedReference,
        field: String,
    },
    MismatchedTypes {
        expected: SpannedReference,
        found: SpannedReference,
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
        found: SpannedReference,
    },
    DeclarationConstraintViolation {
        constraint: DeclarationConstraint,
        found: SpannedReference,
    },
    MissingStructFields {
        parent_struct: SpannedReference,
        fields: Vec<String>,
    },
}

impl Kind {
    pub fn message(&self) -> String {
        match self {
            Self::MismatchedTypes { .. } => "mismatched types".to_string(),
            _ => format!("{self:?}"),
        }
    }

    fn labels<'a>(&self, filename: &'a str, span: Span) -> Vec<Label<(&'a str, Span)>> {
        match self {
            Self::MismatchedTypes { expected, found } => {
                vec![
                    Label::new((filename, expected.span.clone())).with_message("expected this"),
                    Label::new((filename, found.span.clone())).with_message("found this"),
                ]
            }
            _ => vec![Label::new((filename, span)).with_message(format!("{self:?}"))],
        }
    }
}
