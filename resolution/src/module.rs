use memoize::memoize;
use parser::{
    declaration::{self, TopLevel},
    parse, ParseError,
};
use std::{ffi::OsStr, fs, path::Path};

#[derive(Debug, Clone)]
pub enum BuildError {
    Fs(String),
    Path(PathError),
    Parse(ParseError),
}

impl std::fmt::Display for BuildError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Fs(name) => write!(f, "Couldn't read file '{}'", name),
            Self::Path(error) => write!(f, "{}", error),
            Self::Parse(error) => write!(f, "{}", error),
        }
    }
}

#[derive(Debug, Clone)]
pub enum PathError {
    PackageNotAtStart,
}

impl std::fmt::Display for PathError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::PackageNotAtStart => write!(
                f,
                "'package' keyword can only be the first item in import path."
            ),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Module {
    pub dependencies: Vec<Module>,
    pub tree: Vec<TopLevel>,
    pub path: String,
}

#[memoize]
pub fn build(path: String) -> Result<Module, BuildError> {
    let content = fs::read_to_string(&path).map_err(|_| BuildError::Fs(path.to_string()))?;

    let tree = parse(&content).map_err(BuildError::Parse)?;

    let mut dependencies: Vec<Module> = Vec::new();
    let mut new_tree: Vec<TopLevel> = Vec::new();

    for node in tree.into_iter() {
        if let declaration::TopLevel::Import(node) = node {
            let file_path = resolve_path_chunks(&node.path, &path).map_err(BuildError::Path)?;
            let module = build(file_path)?;
            dependencies.push(module);
        } else {
            new_tree.push(node);
        }
    }

    Ok(Module {
        dependencies,
        tree: new_tree,
        path,
    })
}

fn resolve_path_chunks(path: &[String], dependant: &str) -> Result<String, PathError> {
    let dependant_path = Path::new(dependant);
    let dependant_file_name = dependant_path.file_name().unwrap_or_default();
    let mut result_path = String::from(
        dependant_path
            .parent()
            .and_then(Path::to_str)
            .unwrap_or("/"),
    );
    for (index, namespace) in path.iter().enumerate() {
        result_path += &match namespace.as_str() {
            "super" => if dependant_file_name == OsStr::new("index.m") {
                "/.."
            } else {
                "/index.m"
            }
            .to_string(),
            "package" => {
                if index > 0 {
                    return Err(PathError::PackageNotAtStart);
                } else {
                    todo!("Get package root!")
                }
            }
            _ => {
                if Path::new(namespace).is_dir() {
                    format!("/{}/index.m", namespace)
                } else {
                    format!("/{}.m", namespace)
                }
            }
        }
    }
    Ok(dbg!(result_path))
}

/// Resolves `path` relative to `dependant`.
fn resolve_path(path: &str, dependant: &str) -> String {
    let path = Path::new(path);
    if path.is_absolute() {
        path.to_string_lossy().to_string()
    } else {
        let dependant = Path::new(dependant);
        let parent = dependant.parent().unwrap_or_else(|| Path::new("/"));
        parent
            .join(path)
            .into_os_string()
            .to_string_lossy()
            .to_string()
    }
}

// export function add(a: UInt8, b: UInt8) -> a + b;

// 0b1111_1111 + 0b0000_0001 = 0b0000_0000
// a <- 4
// 2 + 2 = 5
