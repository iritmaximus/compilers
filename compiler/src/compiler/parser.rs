use crate::compiler::tokenizer::Token;

/*
* 1. Create structs for all token types
* 2. Make parse-function for ecah token types
* 3. General parse function
* 4. ???
* 5. Profit
*/

enum ExpressionType {
    IntLiteral,
    Identifier,
    BinaryOperator,
}

// T should be equal to ExpressionType
struct Expression<T> {
    expr_type: ExpressionType,
    value: T,
}

struct BinaryOperator<'a, T, K> {
    left: T,
    right: K,
    operator: &'a str,
}

pub fn parser(tokens: Vec<Token>) {
    return;
}
