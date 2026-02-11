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

  def consume(expected: Option[TokenType | List[TokenType] | String | List[String]] = None): Try[Token] =
    val token = peek()

    expected match {
      case Some(that) => {
        that match {
          case expected: String if expected == token.value => {}
          case expected: TokenType if expected == token.tokenType => {}
          case expected: List[String] if expected.contains(token.value) => {}
          case expected: List[TokenType] if expected.contains(token.tokenType) => {}
          case _ => return Failure(new Exception(s"Token ${token} was not expected ${expected}"))
        }
      }
      case None => return Failure(new Exception(s"Trying to parse incorrect expected value: ${expected}"))
    }

    pos += 1
    return Success(token)

  def parseIntLiteral(): Try[Token] =
    return consume(Some(TokenType.IntLiteral))

object Parser:
  def parse(tokens: List[Token]): Try[Expression] =
    val parser = Parser(tokens)
    return Success(Other())
