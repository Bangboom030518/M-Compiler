// TODO: check for circular dependencies
/* TODO: imports
python/rust style???
*/
use memoize::memoize;
use modules::build_module;
use parser::{parse, Declaration, ParseError, Statement};
use std::{fs, path::Path};

mod modules;

pub fn build_file(path: &str) -> Result<Module, ModuleBuildError> {
    build_module(path.to_string())
}
