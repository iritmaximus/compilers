use regex::Regex;

pub fn tokenizer(code: &str) {
    // Damn, regex...
    println!("Doing regex things for {}", code);
    let re = Regex::new(
        r#"(?x)
            (((\#|\/\/).*)|              # Comments
            ([+\-\/*%(),;:{}])|         # Single character operators, e.g. +,-,*, ...
            ([<>=!]=?)|                 # (Potentially) Multicharacter operators, e.g. <,!,=, ... (<=, !=, ==)
            ([0-9]+)|                   # Numbers
            ([a-zA-Z_][a-zA-Z_0-9]*))   # Identifiers
        "#,
    )
    .unwrap();

    let tokens: Vec<&str> = re
        .find_iter(code)
        .map(|mat| {
            let token = mat.as_str();
            println!("{token}");
            token
        })
        .collect();
    println!("{tokens:?}");
    // return tokens;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn single_integer_literals() {
        let code = "143";
        let result = tokenizer(code);

        // assert_eq!(result[0], "143");
    }

    #[test]
    fn multiple_integer_literals() {
        let code = "532 125390";
        let result = tokenizer(code);

        // assert_eq!(result[0], "532");
        // assert_eq!(result[1], "125390");
    }
}
