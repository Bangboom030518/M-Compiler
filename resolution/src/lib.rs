use lazy_static::lazy_static;
use parser::declaration::Import;
use parser::{parse, Declaration, ParseError, Statement};
use std::collections::{hash_map::Entry, HashMap};
use std::fs;
use std::sync::Mutex;

lazy_static! {
    static ref MODULE_CACHE: Mutex<HashMap<String, &'static Module<'static>>> = Mutex::new(HashMap::new());
}

#[derive(Debug)]
pub enum ModuleBuildError {
    FsError,
    ParseError(ParseError),
}

#[derive(Debug)]
pub struct Module<'a> {
    pub dependencies: Vec<&'a Module<'a>>,
    pub tree: Vec<Statement>,
    pub path: String,
}

impl Module<'_> {
    fn try_new<'a>(path: &'a str, dependant: &'a str) -> Result<Box<Self?, ModuleBuildError> {
        let path = resolve_path(path, dependant);
        let mut cache = MODULE_CACHE.lock().expect("Module cache mutex poisoned!");
        let entry = cache.entry(path.to_string());

        if let Entry::Occupied(value) = entry {
            return Ok(value.get());
        }

        build_file(&path);
        let content = match fs::read_to_string(&path) {
            Ok(content) => content,
            Err(_) => return Err(ModuleBuildError::FsError),
        };

        let tree = match parse(&content) {
            Ok(tree) => tree,
            Err(error) => return Err(ModuleBuildError::ParseError(error)),
        };

        let mut dependencies: Vec<&Module> = Vec::new();
        let mut new_tree: Vec<Statement> = Vec::new();

        for node in tree.into_iter() {
            if let Statement::Declaration(Declaration::Import(node)) = node {
                dependencies.push(Self::try_new(&node.path, &path)?);
            } else {
                new_tree.push(node);
            }
        };
        
        Ok(Module {
            dependencies: Vec::new(),
            tree: new_tree,
            path,
        })
    }
}

pub fn build_file(path: &str) -> Result<&Module, ModuleBuildError> {
    Module::try_new(".", path)
}

/// Resolves `path` relative to `dependant`.
fn resolve_path(path: &str, dependant: &str) -> String {
    path.to_string()
}
