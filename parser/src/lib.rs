use tokenizer::Tokenizer;

pub struct Identifier(String);

enum TopLevelDeclarationKind {
    Function(Function),
    Const(Expression),
    // TODO: ?
    Struct(Struct),
    Union(Union)
}

struct TopLevelDeclaration {
    name: Identifier,
    declaration: TopLevelDeclarationKind,
}
