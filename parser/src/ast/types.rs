// TODO: function type

#[derive(Debug)]
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

#[derive(Debug)]
pub struct NamespaceAccess {
    pub left: Box<Type>,
    pub right: Box<Type>,
}

impl NamespaceAccess {
    pub fn new(left: Type, right: Type) -> Self {
        Self {
            left: Box::new(left),
            right: Box::new(right),
        }
    }
}

#[derive(Debug)]
pub enum Type {
    Identifier(String),
    GenericParams(GenericParams),
    NamespaceAccess(
        Vec<Type>
        // NamespaceAccess
    ),
}
