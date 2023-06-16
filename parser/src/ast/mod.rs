pub use identifier::Identifier;
use crate::prelude::*;
use prelude::*;

mod data_type;
mod declarations;
mod expressions;
mod pattern;
mod identifier;

#[derive(Clone, Debug, PartialEq, Eq, Rand)]
#[exclude_test]
pub enum Statement {
    Expression(Expression),
    Declaration(Declaration),
    Continue,
    Break,
    Return(Expression),
}

pub mod prelude {
    pub use super::data_type::*;
    pub use super::declarations::*;
    pub use super::expressions::*;
    pub use super::pattern::Pattern;
    pub use super::Statement;
    pub use super::Identifier;
}
