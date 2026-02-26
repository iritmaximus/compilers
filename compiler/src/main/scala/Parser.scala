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
case class Other() extends Expression {
  override def toString: String = s"Other()"
}

class Parser(tokens: List[Token]):
  // Keeps track of current token, index to tokens: List[Token]
  private var pos = 0

  def peek(): Token =
    return if pos < tokens.length then tokens(pos) else Token("", TokenType.End, tokens.last.location)
    return tokens(pos)

  def consume(expected: Option[TokenType | String | List[String]] = None): Try[Token] =
    val token = peek()

    if token.tokenType == TokenType.Error || token.tokenType == TokenType.End then
      return Failure(new Exception(s"Encountered End or End token: ${token}"))

    expected match {
      case Some(that) => {
        that match {
          case expected: String if expected == token.value => {}
          case expected: TokenType if expected == token.tokenType => {}
          case expected: List[String] if expected.contains(token.value) => {}
          // case expected: List[TokenType] if expected.contains(token.tokenType) => {}
          case _ => return Failure(new Exception(s"Token ${token} was not expected ${expected}"))
        }
      }
      case None => return Failure(new Exception(s"Incorrect expected value: ${expected}"))
    }

    pos += 1
    return Success(token)

  def parseIntLiteral(): Try[Literal] =
    val token = consume(Some(TokenType.IntLiteral))
    return token match {
      case Success(that) => {
        that.value.toIntOption match {
          case Some(int) => Success(Literal(int))
          case None => Failure(new Exception(s"Token value not int: ${token}"))
        }
      }
      case Failure(that) => Failure(that)
    }

  def parseIdentifier(): Try[Identifier] =
    val token = consume(Some(TokenType.Identifier))
    return token match {
      case Success(that) => Success(Identifier(that.value))
      case Failure(that) => Failure(new Exception(s"Incorrect identifier token: ${token}"))
    }

  def parseParenthisized(): Try[Expression] =
    consume(Some("("))
    val expression = parseExpression()
    consume(Some(")"))
    return expression

  def parseFactor(): Try[Expression] =
    val token = peek()
    return token.tokenType match {
      case TokenType.IntLiteral => parseIntLiteral()
      case TokenType.Identifier => parseIdentifier()
      case TokenType.Punctuation if token.value == "(" => parseParenthisized()
      case _ => Failure(new Exception(s"Incorrect token type, expected (, int literal or identifier, got ${token.tokenType} token"))
    }

    
  def parseTerm(): Try[Expression] =
    val operators = List("/", "*")
    var left = parseFactor()

    while
      operators.contains(peek().value)
    do
      val operatorToken = consume(Some(operators))
      val right = parseFactor()
      left = (left, operatorToken, right) match {
        case (Success(l), Success(op), Success(r)) => Success(BinaryOperator(l, op.value, r))
        case _ => Failure(new Exception(s"Failure in some value: l:${left}, op:${operatorToken}, r:${right}"))
      }

    return left match {
      case Success(that) => Success(that)
      case Failure(that) => Failure(new Exception(s"$left"))
    }

  def parseExpression(): Try[Expression] =
    val operators = List("+", "-")
    var left = parseTerm()

    while
      operators.contains(peek().value)
    do
      val operatorToken = consume(Some(operators))
      val right = parseTerm()
      left = (left, operatorToken, right) match {
        case (Success(l), Success(op), Success(r)) => Success(BinaryOperator(l, op.value, r))
        case _ => Failure(new Exception(s"Faiure in some value: l:${left}, op:${operatorToken}, r:${right}"))
      }

    if peek().tokenType != TokenType.End then return Failure(new Exception("Tokens left when there should not be"))

    return left match {
        case Success(that)=> Success(that)
        case Failure(that) => Failure(new Exception(s"Incorrect value ${left}"))
    }


object Parser:
  def parse(tokens: List[Token]): Try[Expression] =
    val parser = Parser(tokens)
    return Success(Other())
