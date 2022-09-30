use annotate_snippets::{
    display_list::{DisplayList, FormatOptions},
    snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation},
};
use peg::{error::ParseError, str::LineCol};

pub struct CompilerError {
    message: String,
}

impl CompilerError {
    pub fn parse_error(error: ParseError<LineCol>, source: &str) -> Self {
        let ParseError {
            location: LineCol { offset, .. },
            expected,
        } = error;

        let expected = format!("expected {}", expected);
        let source = format!("{} ", source);

        let snippet = Snippet {
            title: Some(Annotation {
                label: Some("parse error"),
                id: None,
                annotation_type: AnnotationType::Error,
            }),
            footer: vec![
                Annotation {
                    label: Some(&expected),
                    id: None,
                    annotation_type: AnnotationType::Error,
                },
                Annotation {
                    label: Some("check your syntax"),
                    id: None,
                    annotation_type: AnnotationType::Help,
                },
            ],
            slices: vec![Slice {
                source: &source,
                line_start: 1,
                origin: Some("input"),
                fold: true,
                annotations: vec![SourceAnnotation {
                    label: "",
                    annotation_type: AnnotationType::Error,
                    range: (offset, offset + 1),
                }],
            }],
            opt: FormatOptions {
                ..Default::default()
            },
        };

        Self {
            message: DisplayList::from(snippet).to_string(),
        }
    }
}

impl std::fmt::Display for CompilerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}
