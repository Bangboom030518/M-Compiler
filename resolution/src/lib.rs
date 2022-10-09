use memoize::memoize;
use parser::{parse, Declaration, ParseError, Statement};
use std::fs;

#[derive(Debug, Clone)]
pub enum ModuleBuildError {
    FsError(String),
    ParseError(ParseError),
}

impl std::fmt::Display for ModuleBuildError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::FsError(name) => write!(f, "Couldn't read file {}", name),
            Self::ParseError(error) => write!(f, "{}", error)
        }
    }
}

#[derive(Debug, Clone)]
pub struct Module {
    pub dependencies: Vec<Box<Module>>,
    pub tree: Vec<Statement>,
    pub path: String,
}

impl Module {
    fn try_new(path: String) -> Result<Self, ModuleBuildError> {
        build_module(path)
    }
}

#[memoize]
fn build_module(path: String) -> Result<Module, ModuleBuildError> {
    let content = match fs::read_to_string(&path) {
        Ok(content) => content,
        Err(_) => return Err(ModuleBuildError::FsError(path)),
    };

    let tree = match parse(&content) {
        Ok(tree) => tree,
        Err(error) => return Err(ModuleBuildError::ParseError(error)),
    };

    let mut dependencies: Vec<Module> = Vec::new();
    let mut new_tree: Vec<Statement> = Vec::new();

    for node in tree.into_iter() {
        if let Statement::Declaration(Declaration::Import(node)) = node {
            let module = Module::try_new(resolve_path(&node.path, &path))?;
            dependencies.push(module);
        } else {
            new_tree.push(node);
        }
    }

    Ok(Module {
        dependencies: Vec::new(),
        tree: new_tree,
        path,
    })
}

pub fn build_file(path: &str) -> Result<Module, ModuleBuildError> {
    Module::try_new(path.to_string())
}

/// Resolves `path` relative to `dependant`.
fn resolve_path(path: &str, dependant: &str) -> String {
    path.to_string()
}
