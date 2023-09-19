use crate::utils::{capitalise, parse_key_value_pairs, read_dir, tree, PathEntry};
use lazy_static::lazy_static;
use pulldown_cmark::{html, Options, Parser};
use regex::Regex;
use std::collections::HashMap;
use std::fs;
use std::fs::DirBuilder;
use std::path::Path;

lazy_static! {
    static ref DOCS_DIR_PATTERN: Regex = Regex::new(r"^\./docs").unwrap();
    static ref MARKDOWN_EXT_PATTERN: Regex = Regex::new(r".md$").unwrap();
    static ref MARKDOWN_METADATA_PATTERN: Regex = Regex::new(r"^---\n[\s\S]*?\n---").unwrap();
}

pub fn build() {
    let template = fs::read_to_string("template.html").unwrap();
    let paths = read_dir("./docs");
    let styles = build_styles();
    let nav_tree = build_html_tree(tree("./docs"));

    for path in paths {
        let text = fs::read_to_string(&path).unwrap();
        let dest = MARKDOWN_EXT_PATTERN
            .replace(&DOCS_DIR_PATTERN.replace(&path, "./dist"), ".html")
            .to_string();

        DirBuilder::new()
            .recursive(true)
            .create(Path::new(&dest).parent().unwrap())
            .unwrap();

        let metadata = parse_metadata(&text);
        let filename = get_filename(&dest);
        let markdown = parse_md(&MARKDOWN_METADATA_PATTERN.replace(&text, ""));
        let write_result = fs::write(
            dest,
            template
                .replace(
                    "%heading%",
                    metadata.get("heading").unwrap_or(&filename.to_string()),
                )
                .replace("%content%", &markdown)
                .replace("%styles%", &format!("<style>{styles}</style>"))
                .replace("%nav.tree%", &nav_tree),
        );

        match write_result {
            Ok(_) => (),
            Err(error) => println!("{error}"),
        };
    }
}

fn get_filename(dest: &str) -> String {
    let path = Path::new(dest);
    capitalise(match path.file_stem().unwrap().to_str().unwrap() {
        "index" => match path
            .parent()
            .unwrap()
            .file_name()
            .unwrap()
            .to_str()
            .unwrap()
        {
            "dist" => "M Specification",
            path => path,
        },
        path => path,
    })
}

fn parse_metadata(text: &str) -> HashMap<String, String> {
    let metadata_string = MARKDOWN_METADATA_PATTERN
        .find(text)
        .map_or_else(String::new, |matched| matched.as_str().replace("---", ""));
    parse_key_value_pairs(&metadata_string)
}

fn parse_md(text: &str) -> String {
    let mut options = Options::empty();
    options.insert(Options::ENABLE_STRIKETHROUGH);
    options.insert(Options::ENABLE_TABLES);
    let parser = Parser::new_ext(text, options);

    let mut html_output = String::new();
    html::push_html(&mut html_output, parser);
    html_output
}

fn build_styles() -> String {
    let options = grass::Options::default();
    match grass::from_path(
        "styles/style.scss",
        &options.style(grass::OutputStyle::Compressed),
    ) {
        Ok(value) => value,
        Err(err) => {
            panic!("Sass: {err}");
        }
    }
}

fn build_html_tree(tree: Vec<PathEntry>) -> String {
    let mut result = String::new();
    for path in tree {
        result += &match path {
            PathEntry::Dir(dir) => {
                format!(
                    "<li><details><summary><a href=\"{}\">{}</a></summary><ul>{}</ul></details></li>",
                    &dir.path,
                    &dir.name,
                    &build_html_tree(dir.children)
                )
            }
            PathEntry::File(file) => {
                format!("<li><a href=\"{}\">{}</a></li>", file.path, file.name)
            }
        }
    }
    result
}
