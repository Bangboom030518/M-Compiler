use crate::utils;
use lazy_static::lazy_static;
use regex::Regex;
use std::collections::HashMap;
use std::io::prelude::*;
use std::net::TcpStream;

lazy_static! {
    static ref QUERY_PATTERN: Regex = Regex::new(r"\?.*").unwrap();
    static ref BODY_PATTERN: Regex =
        Regex::new(r"^.+ .+ .+\r\n\r\n(?:.+: .+\r\n)*([\s\S]*?)$").unwrap();
}

pub enum Method {
    Get,
    Post,
    Put,
    Delete,
    Patch,
    Head,
    Connect,
    Options,
    Trace,
}

impl TryFrom<&str> for Method {
    type Error = String;

    fn try_from(string: &str) -> Result<Self, String> {
        match string {
            "GET" => Ok(Self::Get),
            "POST" => Ok(Self::Post),
            "PUT" => Ok(Self::Put),
            "DELETE" => Ok(Self::Delete),
            "PATCH" => Ok(Self::Patch),
            "HEAD" => Ok(Self::Head),
            "CONNECT" => Ok(Self::Connect),
            "OPTIONS" => Ok(Self::Options),
            "TRACE" => Ok(Self::Trace),
            method => Err(format!("Invalid HTTP Method '{method}'")),
        }
    }
}

pub struct Request {
    pub headers: HashMap<String, String>,
    pub method: Method,
    pub body: String,
    pub route: String,
    pub query: HashMap<String, String>,
}

pub fn parse(mut stream: &TcpStream) -> Request {
    let mut buffer = [0; 1024];

    stream.read_exact(&mut buffer).unwrap();

    let raw_request = String::from_utf8_lossy(&buffer[..]);
    let mut lines: Vec<&str> = raw_request.split("\r\n").collect();

    let request_line: Vec<&str> = lines[0].split_whitespace().collect();

    lines.remove(0);
    let headers = utils::parse_key_value_pairs(&raw_request);
    let body = BODY_PATTERN
        .captures(&raw_request)
        .map_or_else(String::new, |captures| {
            captures.get(0).map_or("", |m| m.as_str()).to_string()
        });

    Request {
        headers,
        route: if request_line.len() == 3 {
            QUERY_PATTERN.replace_all(request_line[1], "").to_string()
        } else {
            "/".to_string()
        },
        method: Method::try_from(request_line[0]).unwrap(),
        body,
        query: HashMap::new(),
    }
}
