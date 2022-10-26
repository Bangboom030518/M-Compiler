// TODO: function type

#[derive(Debug, Clone)]
pub struct Params {
    pub operand: Box<Type>,
    pub arguments: Vec<Type>,
}

impl Params {
    #[must_use]
    pub fn new(operand: Type, arguments: Vec<Type>) -> Self {
        Self {
            operand: Box::new(operand),
            arguments,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Type {
    Identifier(super::Identifier),
    Params(Params),
    NamespaceAccess(Vec<Type>),
}
