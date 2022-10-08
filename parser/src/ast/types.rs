// TODO: function type

#[derive(Debug, Clone)]
pub struct GenericParams {
    pub operand: Box<Type>,
    pub arguments: Vec<Type>,
}

impl GenericParams {
    pub fn new(operand: Type, arguments: Vec<Type>) -> Self {
        Self {
            operand: Box::new(operand),
            arguments,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Type {
    Identifier(String),
    GenericParams(GenericParams),
    NamespaceAccess(Vec<Type>),
}
