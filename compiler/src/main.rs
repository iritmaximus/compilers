use base64::prelude::*;
use std::{
    io::{BufReader, prelude::*},
    net::{TcpListener, TcpStream},
};

pub mod compiler;

const PORT: &str = "3000";
const HOST: &str = "0.0.0.0";

fn main() {
    let address = format!("{}:{}", HOST, PORT);
    let listener = TcpListener::bind(&address).unwrap();

    println!("Starting server at address {}", address);

    for stream in listener.incoming() {
        let stream = stream.unwrap();

        handle_connection(stream);
    }
}

fn handle_connection(mut stream: TcpStream) {
    let buf_reader = BufReader::new(&stream);
    let http_request: Vec<_> = buf_reader
        .lines()
        .map(|result| result.unwrap())
        .take_while(|line| !line.is_empty())
        .collect();

    let body = http_request.last().unwrap();

    if body.is_empty() {
        eprintln!("Empty body!");
        let response = "HTTP/1.1 404 NOT FOUND";
        stream.write_all(response.as_bytes()).unwrap();
        return;
    }

    let json = json::parse(body).unwrap();
    let command = json["command"].as_str().unwrap();

    println!("Request: {http_request:#?}");
    // println!("Body: {json:#?}");
    println!("Command: {command:#?}");

    match command {
        "ping" => {
            println!("Pong!");
            let response = "{}";
            stream.write_all(response.as_bytes()).unwrap();
        }
        "compile" => {
            let code = json["code"].as_str().unwrap();
            let executable = compiler::compile(code);
            let content = BASE64_STANDARD.encode(executable);

            let response = format!("{{\"program\": \"{}\"}}", content.as_str());
            println!("Response: {:?}", response);
            stream.write_all(response.as_bytes()).unwrap();
        }
        _ => {
            let response = "HTTP/1.1 404 NOT FOUND";
            stream.write_all(response.as_bytes()).unwrap();
        }
    }
}
