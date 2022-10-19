// TODO: implement spanned on everything

pub mod data_type;
pub mod declaration;
pub mod expression;

pub use data_type::Type;
pub use declaration::Declaration;
pub use expression::Expression;

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(expression::Expression),
    Declaration(declaration::Declaration),
}
