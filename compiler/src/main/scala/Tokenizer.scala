package compiler.Tokenizer

import scala.util.matching.Regex.MatchIterator
import scala.util.{Try, Failure, Success}


val TabLength = 4


class Position(var line: Int, var column: Int):
  override def toString: String = s"($line,$column)"
  override def equals(other: Any): Boolean = other match {
    case that: Position => (that.line == this.line) && (that.column == this.column)
    case _ => false
  }

class TokenLocation(val start: Position, val end: Position):
  override def toString: String = s"TokenLoc(S:$start, E:$end)"
  override def equals(other: Any): Boolean = other match {
    case that: TokenLocation => (that.start == this.start) && (that.end == this.end)
    case that: TokenLocationDebug => true
    case _ => false
  }

class TokenLocationDebug():
  override def toString: String = s"(debug-loc)"
  override def equals(other: Any): Boolean = other match {
    case that: TokenLocation => true
    case that: TokenLocationDebug => true
    case _ => false
  }

sealed trait Token(val value: String, val location: TokenLocation | TokenLocationDebug = TokenLocationDebug()):
  override def toString: String = s"${this.getClass.getName.stripPrefix("compiler.Tokenizer.")}($value, $location)"
  override def equals(other: Any): Boolean = other match {
    case that: Token => {
      that.location match {
        case x: TokenLocation => (that.value == this.value) && (that.location == this.location)
        case x: TokenLocationDebug => true
      }
    }
    case _ => false
  }
  def isSkippable() =
    this match {
      case that: WhitespaceT => true
      case that: CommentT => true
      case _ => false
    }

object Token:
  def tokenFromString(str: String, currentPos: Position): Option[Token] =


    if Token.isWhitespace(str) then 
      val startWhitespace = Position(currentPos.line, currentPos.column)
      str.foreach(_ match {
        case ' ' => currentPos.column += 1
        case '\n' => currentPos.line += 1; currentPos.column = 1
        case '\t' => currentPos.column += TabLength
      })

      val endWhitespace = Position(currentPos.line, currentPos.column)
      val tokenLoc = TokenLocation(startWhitespace, endWhitespace)
      return Some(WhitespaceT(str, tokenLoc))

    // After whitespace is calculated, move currentPos to the end of the current token
    // i.e. current column + token length
    val start = Position(currentPos.line, currentPos.column)
    currentPos.column += str.length()
    val end = Position(currentPos.line, currentPos.column)
    val tokenLoc = TokenLocation(start, end)

    if Token.isComment(str) then return Some(CommentT(str, tokenLoc))
    if Token.isIntLiteral(str) then return Some(IntLiteralT(str, tokenLoc))
    if Token.isOperator(str) then return Some(OperatorT(str, tokenLoc))
    if Token.isIdentifier(str) then return Some(IdentifierT(str, tokenLoc))
    if Token.isPunctuation(str) then return Some(PunctuationT(str, tokenLoc))
    else return None


  def isWhitespace(str: String): Boolean = return raw"(\s+)".r.matches(str)
  def isComment(str: String): Boolean = return str.startsWith("#") || str.startsWith("//")
  def isIntLiteral(str: String): Boolean = return str.forall(Character.isDigit)
  def isOperator(str: String): Boolean = return raw"([+\-\/*%])|([<>=!]=?)".r.matches(str)
  def isPunctuation(str: String): Boolean = raw"[(),;:{}]".r.matches(str)
  def isIdentifier(str: String): Boolean = raw"[a-zA-Z_][a-zA-Z_0-9]*".r.matches(str)

class WhitespaceT(value: String, location: TokenLocation | TokenLocationDebug) extends Token(value, location)
class CommentT(value: String, location: TokenLocation | TokenLocationDebug) extends Token(value, location)
class IntLiteralT(value: String, location: TokenLocation | TokenLocationDebug) extends Token(value, location)
class OperatorT(value: String, location: TokenLocation | TokenLocationDebug) extends Token(value, location)
class PunctuationT(value: String, location: TokenLocation | TokenLocationDebug) extends Token(value, location)
class IdentifierT(value: String, location: TokenLocation | TokenLocationDebug) extends Token(value, location)
class OtherT(value: String, location: TokenLocation | TokenLocationDebug) extends Token(value, location)


object Tokenizer:
  def tokenize(source: String): Try[List[Token]] =
    val tokenRegex =
      raw"(\s+)|(((?:\#|\/\/).*$$)|([+\-\/*%])|([(),;:{}])|([<>=!]=?)|([0-9]+)|([a-zA-Z_][a-zA-Z_0-9]*))".r
    val matches = tokenRegex.findAllIn(source)

    var currentPos = Position(1, 1)
    val tokens = matches.map(token => Token.tokenFromString(token, currentPos)).toList

    if tokens.exists(_.isEmpty) 
      then Failure(new Exception("Incorrect tokens found"))
    else
      Success(tokens.flatten.filter(!_.isSkippable()))
