use lazy_static::lazy_static;
use regex::Regex;
use std::collections::HashMap;
use std::fs;

lazy_static! {
    static ref NEW_LINES_PATTERN: Regex = Regex::new(r"\r\n|\n").unwrap();
}

pub fn parse_key_value_pairs(text: &str) -> HashMap<String, String> {
    let mut result: HashMap<String, String> = HashMap::new();
    let lines: Vec<&str> = NEW_LINES_PATTERN.split(text).collect();
    for line in lines {
        if line.contains(": ") {
            let split: Vec<&str> = line.split(": ").collect();
            result.insert(split[0].to_string(), split[1].to_string());
        };
    }
    result
}

pub fn capitalise(text: &str) -> String {
    let mut chars = text.chars();
    chars.next().map_or_else(String::new, |char| {
        char.to_uppercase().collect::<String>() + chars.as_str()
    })
}

pub fn read_dir(path: &str) -> Vec<String> {
    let paths = fs::read_dir(path).unwrap();
    let mut result: Vec<String> = Vec::new();
    for path in paths {
        let path = path.unwrap().path();
        if path.is_dir() {
            result.append(&mut read_dir(&path.display().to_string()));
        }
        if path.is_file() {
            result.push(path.display().to_string());
        }
    }
    result
}

pub struct Dir {
    pub children: Vec<PathEntry>,
    pub path: String,
    pub name: String,
}

pub struct File {
    pub path: String,
    pub name: String,
}

pub enum PathEntry {
    File(File),
    Dir(Dir),
}

pub fn tree(path: &str) -> Vec<PathEntry> {
    let paths = fs::read_dir(path).unwrap();
    let mut result: Vec<PathEntry> = Vec::new();
    for path in paths {
        let path = path.unwrap().path();
        let is_dir = path.is_dir();
        let name = path.file_stem().unwrap().to_str().unwrap().to_string();
        let path = path.to_str().unwrap().to_string();
        result.push(if is_dir {
            PathEntry::Dir(Dir {
                children: tree(&path),
                name,
                path,
            })
        } else {
            PathEntry::File(File { path, name })
        });
    }
    result
}
