// TODO: check for circular dependencies
/* TODO: imports
python/rust style???
*/


mod module;

pub fn build_file(path: &str) -> Result<module::Module, module::BuildError> {
    module::build(path.to_string())
}
