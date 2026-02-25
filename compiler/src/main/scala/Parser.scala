package compiler.Parser

import scala.util.{Try, Success, Failure}

import compiler.Tokenizer.{Token, TokenType}

sealed abstract class Expression:
  //override def toString() = s"${this.getClass.getName.stripPrefix("compiler.Parser.")}($this)"
  override def toString() = s"Expression(TODO)"


case class Literal(value: Int | Boolean) extends Expression
case class Identifier(name: String) extends Expression
case class BinaryOperator(left: Expression, operator: String, right: Expression) extends Expression
case class Other() extends Expression

class Parser(tokens: List[Token]):
  // Keeps track of current token, index to tokens: List[Token]
  private var pos = 0

  def peek(): Token =
    return tokens(pos)

  def consume(expected: Option[TokenType | String | List[String]] = None): Try[Token] =
    val token = peek()

    if token.tokenType == TokenType.Error then
      return Failure(new Exception(s"Encountered Error token: ${token}"))

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
      case None => return Failure(new Exception(s"Trying to parse incorrect expected value: ${expected}"))
    }

    pos += 1
    return Success(token)

  def parseIntLiteral(): Try[Literal] =
    val token = consume(Some(TokenType.IntLiteral))
    return token match {
      case Success(that) => {
        that.value.toIntOption match {
          case Some(int) => Success(Literal(int))
          case None => Failure(new Exception(s"Value of token was not int: ${token}"))
        }
      }
      case Failure(that) => Failure(that)
    }

  def parseExpression(): Try[BinaryOperator] =
    val left = parseIntLiteral()
    val operatorToken = consume(Some(List("+", "-")))
    val right = parseIntLiteral()

    (left, operatorToken, right) match {
      case (Success(l), Success(op), Success(r)) => return Success(BinaryOperator(l, op.value, r))
      case _ => Failure(new Exception(s"Trying to parse BinaryOperator but failed with values ${left} ${operatorToken} ${right}"))
    }


    


object Parser:
  def parse(tokens: List[Token]): Try[Expression] =
    val parser = Parser(tokens)
    return Success(Other())
