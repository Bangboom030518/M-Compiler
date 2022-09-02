use lazy_static::lazy_static;
use regex::Regex;

lazy_static! {
    static ref COMMENTS_PATTERN: Regex = Regex::new(r"/\*[\s\S]*\*/|//.*").expect("Couldn't parse COMMENTS_PATTERN regex");
}

fn parse(content: &str) {
    let content = remove_comments(content);
    let lines = content.lines().filter(|&line| !line.is_empty());
}

pub fn validate_pest(content: &str) -> Result<(), String> {
    parse(content);
    Ok(())
}

fn remove_comments(content: &str) -> String {
    COMMENTS_PATTERN.replace_all(content, "").to_string()
}