package compiler.Tokenizer;

import scala.util.matching.Regex.MatchIterator;

class TokenLocation(column: Int, line: Int):
  override def toString: String = s"($line, $column)"

class TokenLocationDebug():
  override def toString: String =
    s"(debug-loc)"

class Token(
    value: String,
    location: TokenLocation | TokenLocationDebug = TokenLocationDebug()
):
  override def toString: String =
    s"Token($value, $location)"

object Token:
  def tokenFromString(str: String): Token =
    return Token("test")
  
  def isIntLiteral(str: String): Boolean =
    return true

class CurrentPosition(line: Int, column: Int);

def tokenizer(source: String): List[Token] =
  val tokenRegex =
    raw"(((\#|\/\/).*)|([+\-\/*%(),;:{}])|([<>=!]=?)|([0-9]+)|([a-zA-Z_][a-zA-Z_0-9]*))".r
  val matches = tokenRegex.findAllIn(source)

  var currentPos = CurrentPosition(0, 0)
  val tokens = for (token <- matches) yield tokenFromMatch(token, currentPos)

  return tokens.toList

def tokenFromMatch(tokenStr: String, curPos: CurrentPosition): Token =
  val location = TokenLocationDebug()
  val value = tokenStr

  val token = Token(value, location)
  println("Token: " + token)
  return token
