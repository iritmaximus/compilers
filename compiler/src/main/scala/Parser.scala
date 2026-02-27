package compiler.Parser

import scala.util.{Try, Success, Failure}

import compiler.Tokenizer.{Token, TokenType}

sealed abstract class Expression:
  override def toString() = s"Expression(TODO)"


case class Literal(value: Int | Boolean) extends Expression {
  override def toString: String = s"Literal($value)"
}
case class Identifier(name: String) extends Expression {
  override def toString: String = s"Identifier($name)"
}
case class BinaryOperator(left: Expression, operator: String, right: Expression) extends Expression {
  override def toString: String = s"BinaryOperator(l: $left, op: $operator, r: $right)"
}
// case class ControlFlow(name: String, condition: Expression, body: Expression, elseBody: Option[Expression]) {
//   override def toString: String = s"ControlFlow($name $condition: $body)"
// }
case class Other() extends Expression {
  override def toString: String = s"Other()"
}

class Parser(tokens: List[Token]):
  // Keeps track of current token, index to tokens: List[Token]
  private var pos = 0

  def peek(): Token =
    return if pos < tokens.length then tokens(pos) else Token("", TokenType.End, tokens.last.location)

  def consume(expected: Option[TokenType | String | List[String]] = None): Try[Token] =
    val token = peek()

    if token.tokenType == TokenType.Error || token.tokenType == TokenType.End then
      return Failure(new Exception(s"Encountered Error or End token: ${token}"))

    expected match {
      case Some(that) => {
        that match {
          case expected: String if expected == token.value => {}
          case expected: TokenType if expected == token.tokenType => {}
          case expected: List[String] if expected.contains(token.value) => {}
          // case expected: List[TokenType] if expected.contains(token.tokenType) => {}
          case _ => return Failure(new Exception(s"Token ${token} was not expected: ${that}"))
        }
      }
      case None => return Failure(new Exception(s"Incorrect expected value: ${expected}"))
    }

    pos += 1
    return Success(token)

  def parseIntLiteral(): Literal =
    val token = consume(Some(TokenType.IntLiteral))
    return token match {
      case Success(that) => that.value.toIntOption match {
        case Some(int) => Literal(int)
        case None => throw new Exception(s"Token value not int: ${that.value}")
      }
      case Failure(that) => throw that
    }

  def parseIdentifier(): Identifier =
    val token = consume(Some(TokenType.Identifier))
    return token match {
      case Success(that) => Identifier(that.value)
      case Failure(that) => throw that 
    }

  def parseParenthisized(): Expression =
    // Throw error if consume returns Failure
    consume(Some("(")).get
    val expression = parseExpression()
    consume(Some(")")).get
    return expression

  def parseFactor(): Expression =
    val token = peek()
    return token.tokenType match {
      case TokenType.IntLiteral => parseIntLiteral()
      case TokenType.Identifier => parseIdentifier()
      case TokenType.Punctuation if token.value == "(" => parseParenthisized()
      case _ => throw new Exception(s"Incorrect token type: Expected (, int literal or identifier, got ${token.tokenType} token")
    }

  def parseTerm(): Expression =
    val operators = List("/", "*")
    var left = parseFactor()

    while
      operators.contains(peek().value)
    do
      val operatorToken = consume(Some(operators)).get
      val right = parseFactor()
      left = BinaryOperator(left, operatorToken.value, right)

    return left

  def parseExpression(): Expression =
    val operators = List("+", "-")
    var left = parseTerm()

    while
      val operatorToken = consume(Some(operators)).get
      val right = parseTerm()
      left = BinaryOperator(left, operatorToken.value, right)
      operators.contains(peek().value)
    do ()

    if peek().tokenType != TokenType.End then throw new Exception("Tokens left when there should not be")

    return left

object Parser:
  def parse(tokens: List[Token]): Try[Expression] =
    val parser = Parser(tokens)
    return Success(Other())
