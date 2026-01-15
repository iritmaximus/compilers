use regex::Regex;

pub fn tokenizer(code: &str) -> Vec<&str> {
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
            // Add line number and location to token
            let token = mat.as_str();
            println!("{token}");
            token
        })
        .collect();
    println!("{tokens:?}");
    return tokens;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn single_integer_literals() {
        let code = "143";
        assert_eq!(tokenizer(code), ["143"]);
    }
    #[test]
    fn multiple_integer_literals() {
        let code = "532 125390";
        assert_eq!(tokenizer(code), ["532", "125390"]);
    }

    #[test]
    fn comment_hashtag() {
        let code = "# This is a comment :)";
        let compare: Vec<&str> = vec![code];
        assert_eq!(tokenizer(code), compare);
    }
    #[test]
    fn comment_div() {
        let code = "// This is a comment :)";
        let compare: Vec<&str> = vec![code];
        assert_eq!(tokenizer(code), compare);
    }

    #[test]
    fn math_operators() {
        let code = "1 + 2 * 5";
        let compare: Vec<&str> = vec!["1", "+", "2", "*", "5"];
        assert_eq!(tokenizer(code), compare);
    }
    #[test]
    fn identifiers_with_whitespace() {
        let code = "if  3\nwhile";
        let compare: Vec<&str> = vec!["if", "3", "while"];
        assert_eq!(tokenizer(code), compare);
    }

    #[test]
    fn more_complex_comxinations() {
        let code = "if a <= bee then print_int(123)";
        let compare: Vec<&str> = vec!["if", "a", "<=", "bee", "then", "print_int", "(", "123", ")"];
        assert_eq!(tokenizer(code), compare);
    }
}
