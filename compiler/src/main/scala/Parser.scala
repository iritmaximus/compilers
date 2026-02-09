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

  def consume(expected: Option[TokenType | List[TokenType]] = None): Token =
    val token = peek()

    expected match {
      case Some(types) => types match {
        case that: TokenType => println("IMPLEMENT TOKENTYPE")
        case that: List[TokenType] => println("IMPLEMENT LIST[TOKENTYPE]")
      }
      case None => println("IMPLEMENT NONE")
    }
    
    pos += 1
    return tokens(pos)

  def handleExpected(token: Token, expected: TokenType): Try[Token] =
    return Success(token)


object Parser:
  def parse(tokens: List[Token]): Try[Expression] =
    val parser = Parser(tokens)
    return Success(Other())
