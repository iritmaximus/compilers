package compiler.Parser

import scala.util.{Try, Success, Failure}

import compiler.Tokenizer.{Token, TokenType}

sealed trait Expression:
  //override def toString() = s"${this.getClass.getName.stripPrefix("compiler.Parser.")}($this)"
  override def toString() = s"Expression(TODO)"


class Literal(value: Int | Boolean) extends Expression
class Identifier(name: String) extends Expression
class BinaryOperator(left: Expression, operator: String, right: Expression) extends Expression
class Other() extends Expression

class Parser(tokens: List[Token]):
  // Keeps track of current token, index to tokens: List[Token]
  private var pos = 0

  def peek(): Token =
    return tokens(pos)

  def consume(expected: TokenType | List[TokenType]): Token =
    val token = peek()
    
    pos += 1
    return tokens(pos)


object Parser:
  def parse(tokens: List[Token]): Try[Expression] = return Success(Other())
