use crate::http::status::{Code as StatusCode};
use std::collections::HashMap;

pub struct Response {
    pub headers: HashMap<String, String>,
    pub body: String,
    pub status_code: StatusCode,
}

impl Default for Response {
    fn default() -> Self {
        Self {
            headers: HashMap::new(),
            body: String::new(),
            status_code: StatusCode::OK,
        }
    }
}

pub fn create(mut response: Response) -> Vec<u8> {
    response.headers.insert(
        "Content-Length".to_string(),
        (response.body.len() + 2).to_string(),
    );
    let headers = response
        .headers
        .iter()
        .map(|(name, value)| format!("{name}: {value}\r\n"))
        .collect::<String>();

    format!(
        "HTTP/1.1 {}\r\n{}\r\n\r\n{}",
        response.status_code.as_str(),
        headers,
        response.body
    )
    .as_bytes()
    .to_owned()
}
