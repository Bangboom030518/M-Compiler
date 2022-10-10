// TODO: check for circular dependencies
/* TODO: imports
python/rust style???
*/
use memoize::memoize;
use parser::{parse, Declaration, ParseError, Statement};
use std::{fs, path::Path};

#[derive(Debug, Clone)]
pub enum ModuleBuildError {
    FsError(String),
    ParseError(ParseError),
}

impl std::fmt::Display for ModuleBuildError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::FsError(name) => write!(f, "Couldn't read file '{}'", name),
            Self::ParseError(error) => write!(f, "{}", error),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Module {
    pub dependencies: Vec<Module>,
    pub tree: Vec<Statement>,
    pub path: String,
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
            let module = build_module(resolve_path(&node.path, &path))?;
            dependencies.push(module);
        } else {
            new_tree.push(node);
        }
    }

    // dbg!(dependencies);

    Ok(Module {
        dependencies,
        tree: new_tree,
        path,
    })
}

pub fn build_file(path: &str) -> Result<Module, ModuleBuildError> {
    build_module(path.to_string())
}

/// Resolves `path` relative to `dependant`.
fn resolve_path(path: &str, dependant: &str) -> String {
    let path = Path::new(path);
    if path.is_absolute() {
        return path.to_string_lossy().to_string();
    }
    let dependant = Path::new(dependant);
    let parent = dependant.parent().unwrap_or_else(|| Path::new("/"));
    parent.join(path).into_os_string().to_string_lossy().to_string()
}
