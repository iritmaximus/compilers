use lazy_static::lazy_static;
use regex::Regex;

const WHITESPACE_REGEX_STR: &str = r"(\n|\r\n|\r|\\n|\ )";
const COMMENT_REGEX_STR: &str = r"(((\#|\/\/).*)";
const SINGLE_CHAR_OPERATOR_STR: &str = r"([+\-\/*%(),;:{}])";
const MULTICHAR_OPERATOR_STR: &str = r"([<>=!]=?)";
const NUMBER_LITERAL_STR: &str = r"([0-9]+)";
const IDENTIFIER_STR: &str = r"([a-zA-Z_][a-zA-Z_0-9]*))";

lazy_static! {
    // Damn, regex...
    static ref TOKEN_RE: Regex = Regex::new(format!(r#"{WHITESPACE_REGEX_STR}|{COMMENT_REGEX_STR}|{SINGLE_CHAR_OPERATOR_STR}|{MULTICHAR_OPERATOR_STR}|{NUMBER_LITERAL_STR}|{IDENTIFIER_STR}"#).as_str()).unwrap();
    static ref WHITESPACE_RE: Regex = Regex::new(format!(r#"{WHITESPACE_REGEX_STR}"#).as_str()).unwrap();
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TokenLocation {
    line: u64,
    column: u64,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TokenType {
    Identifier,
    IntLiteral,
    Other,
}

#[derive(Debug, Eq)]
pub struct Token<'a> {
    location: TokenLocation,
    token_type: TokenType,
    value: &'a str,
}

impl<'a> PartialEq for Token<'a> {
    fn eq(&self, other: &Token) -> bool {
        if self.location == other.location
            && self.value == other.value
            && self.token_type == other.token_type
        {
            return true;
        }
        return false;
    }
}

fn is_whitespace(token: &str) -> bool {
    return WHITESPACE_RE.is_match(token);
}

pub fn tokenizer(code: &str) -> Vec<Token<'_>> {
    println!("Doing regex things for {:?}", code);

    let mut tokens_vec: Vec<Token> = vec![];
    let mut current_pos = TokenLocation { column: 0, line: 0 };
    for mat in TOKEN_RE.find_iter(code) {
        println!();
        let token = mat.as_str();
        if is_whitespace(token) {
            match token {
                "\n" | "\r\n" | "\r" => {
                    current_pos.column = 0;
                    current_pos.line += 1;
                }
                // This type cast can fail/truncate <usize> on 128-bit machines, still waiting for those :D
                _ => current_pos.column += token.len() as u64,
            }
            continue;
        }

        // Same as above about 128-bit machines
        current_pos.column += token.len() as u64;

        let token_pos = current_pos;
        let token_obj = Token {
            location: token_pos,
            token_type: TokenType::Other,
            value: token,
        };

        tokens_vec.push(token_obj);
    }

    let tokens: Vec<&str> = TOKEN_RE
        .find_iter(code)
        .map(|mat| {
            // Add line number and location to token
            let token = mat.as_str();
            println!("{token}");
            token
        })
        .collect();
    println!("{tokens:?}");
    println!("{tokens_vec:?}");
    return tokens_vec;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn single_integer_literals() {
        let code = "143";
        let result = [Token {
            location: TokenLocation { column: 3, line: 0 },
            token_type: TokenType::IntLiteral,
            value: "143",
        }];
        assert_eq!(tokenizer(code)[0], result[0]);
    }
    #[test]
    fn multiple_integer_literals() {
        let code = "532 125390";
        let result = [
            Token {
                location: TokenLocation { column: 3, line: 0 },
                token_type: TokenType::IntLiteral,
                value: "532",
            },
            Token {
                location: TokenLocation {
                    column: 10,
                    line: 0,
                },
                token_type: TokenType::IntLiteral,
                value: "125390",
            },
        ];
        assert_eq!(tokenizer(code), result);
    }

    #[test]
    fn integer_literals_with_newline() {
        let code = "123 \n3210\n\n50";
        let result = [
            Token {
                location: TokenLocation { column: 3, line: 0 },
                token_type: TokenType::Other,
                value: "123",
            },
            Token {
                location: TokenLocation { column: 4, line: 1 },
                token_type: TokenType::Other,
                value: "3210",
            },
            Token {
                location: TokenLocation { column: 2, line: 3 },
                token_type: TokenType::Other,
                value: "50",
            },
        ];

        assert_eq!(tokenizer(code), result);
    }

    #[test]
    fn comment_hashtag() {
        let code = "# This is a comment :)";
        assert_eq!(tokenizer(code).len(), 0);
    }
    #[test]
    fn comment_div() {
        let code = "// This is a comment :)";
        assert_eq!(tokenizer(code).len(), 0);
    }

    #[test]
    fn math_operators() {
        let code = "1 + 2 * 5";
        let tokens: Vec<&str> = tokenizer(code).iter().map(|token| token.value).collect();
        let result: Vec<&str> = vec!["1", "+", "2", "*", "5"];
        assert_eq!(tokens, result);
    }
    #[test]
    fn identifiers_with_whitespace() {
        let code = "if  3\nwhile";
        let tokens: Vec<&str> = tokenizer(code).iter().map(|token| token.value).collect();
        let compare: Vec<&str> = vec!["if", "3", "while"];
        assert_eq!(tokens, compare);
    }

    #[test]
    fn more_complex_comxinations() {
        let code = "if a <= bee then print_int(123)";
        let tokens: Vec<&str> = tokenizer(code).iter().map(|token| token.value).collect();
        let compare: Vec<&str> = vec!["if", "a", "<=", "bee", "then", "print_int", "(", "123", ")"];
        assert_eq!(tokens, compare);
    }
}
