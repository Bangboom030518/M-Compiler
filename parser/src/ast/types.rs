// TODO: function type

#[derive(Debug)]
pub struct GenericParams {
    pub arguments: Vec<Type>,
    pub callable: Box<Type>
}

#[derive(Debug)]
pub enum Type {
    Identifier(String),
    GenericParams(GenericParams),
}
