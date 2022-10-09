// TODO: implement spanned on everything

pub mod declaration;
pub mod expression;
pub mod types;

pub use declaration::Declaration;
pub use expression::Expression;
pub use types::Type;

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(expression::Expression),
    Declaration(declaration::Declaration),
}
