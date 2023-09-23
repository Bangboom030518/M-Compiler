use tokenizer::Tokenizer;

pub struct Identifier(String);

pub enum Type {
    Identifier(Identifier)
}

// TODO: rename
struct Field {
    r#type: Type,
    name: Identifier,
}

enum TypeDeclarationKind {
    Struct,
    Union,
}

struct TypeDeclaration {
    fields: Vec<Field>,
    declarations: Vec<TopLevelDeclaration>,
    kind: TypeDeclarationKind,
}

enum Body {
    Single(Expression),
    Multiple(Vec<Expression>)
}

struct Function {
    parameters: Vec<Field>,
    return_type: Type,
    body: Body
}

enum Expression {
    Identifier(Identifier)
}

enum TopLevelDeclarationKind {
    Function(Function),
    Const(Expression),
    // TODO: ?
    Type(TypeDeclaration),
}

struct TopLevelDeclaration {
    name: Identifier,
    declaration: TopLevelDeclarationKind,
}
