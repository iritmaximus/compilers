use base64::prelude::*;
use std::{
    io::{BufReader, prelude::*},
    net::{TcpListener, TcpStream},
};

const PORT: &str = "4000";
const HOST: &str = "127.0.0.1";

fn main() {
    let address = format!("{}:{}", HOST, PORT);
    let listener = TcpListener::bind(&address).unwrap();

    println!("Starting server at address {}", address);

    for stream in listener.incoming() {
        let stream = stream.unwrap();

        handle_connection(stream);
    }
}

fn compile(code: &str) -> &str {
    println!("Code: {}", &code);
    return code;
}

fn handle_connection(mut stream: TcpStream) {
    let buf_reader = BufReader::new(&stream);
    let http_request: Vec<_> = buf_reader.lines().map(|result| result.unwrap()).collect();

    let body = http_request.last().unwrap();

    let json = json::parse(body).unwrap();
    let command = json["command"].as_str().unwrap();

    println!("Request: {http_request:#?}");
    println!("Body: {json:#?}");
    println!("Command: {command:#?}");

    match command {
        "ping" => {
            println!("Pong!");
            let response = "HTTP/1.1 200 OK";
            stream.write_all(response.as_bytes()).unwrap();
        }
        "compile" => {
            let code = json["code"].as_str().unwrap();
            let executable = compile(code);
            let content = BASE64_STANDARD.encode(executable);
            let length = content.len();

            let response = format!("HTTP/1.1 200 OK\rContent-Length: {length}\r\n\r{content}");
            stream.write_all(response.as_bytes()).unwrap();
        }
        _ => {
            let response = "HTTP/1.1 404 NOT FOUND";
            stream.write_all(response.as_bytes()).unwrap();
        }
    }
}
