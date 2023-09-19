#![warn(clippy::nursery, clippy::pedantic)]
use crate::build::build;
use lazy_static::lazy_static;
use regex::Regex;
use std::fs;
use std::io::prelude::*;
use std::net::{TcpListener, TcpStream};
use std::path::Path;

mod build;
mod http;
mod utils;

const PORT: u16 = 80;

lazy_static! {
    static ref TRAILING_SLASH_PATTERN: Regex = Regex::new(r"/?$").unwrap();
    static ref ADDRESS: String = format!("127.0.0.1:{PORT}");
}

// https://stackoverflow.com/questions/19650265/is-there-a-faster-shorter-way-to-initialize-variables-in-a-rust-struct

// TODO: multithreading???
// TODO: typescript using SWC?

fn main() {
    build();

    let listener = TcpListener::bind(&*ADDRESS).unwrap();

    println!("Server running on http://{}", *ADDRESS);

    for stream in listener.incoming() {
        let stream = stream.unwrap();
        println!("Connection established!");
        handle_connection(stream);
    }
}

fn handle_connection(mut stream: TcpStream) {
    let request = http::request::parse(&stream);
    let route = match &request.route[..] {
        route if Path::new(&format!("./dist/{route}")).is_dir() => {
            format!("{}/index", TRAILING_SLASH_PATTERN.replace("", route))
        }
        route => route.to_string(),
    };
    println!("{}", TRAILING_SLASH_PATTERN.replace("", &request.route[..]));
    let path = format!("./dist{route}.html");
    let response = http::response::create(match path {
        _ if Path::new(&path).is_file() => http::response::Response {
            body: fs::read_to_string(&path).unwrap(),
            ..Default::default()
        },
        _ => http::response::Response {
            body: fs::read_to_string("404.html").unwrap(),
            status_code: http::status::Code::NotFound,
            ..Default::default()
        },
    });

    stream.write_all(&response).unwrap();
    stream.flush().unwrap();
}

