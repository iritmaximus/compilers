pub mod parser;
pub mod tokenizer;

pub fn compile(code: &str) -> &str {
    tokenizer::tokenizer(code);
    println!("Code: {}", &code);
    return code;
}
