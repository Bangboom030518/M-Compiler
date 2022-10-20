// TODO: check for circular dependencies
/* TODO: imports
python/rust style???
*/
use modules::build_module;

mod modules;

pub fn build_file(path: &str) -> Result<Module, ModuleBuildError> {
    build_module(path.to_string())
}
