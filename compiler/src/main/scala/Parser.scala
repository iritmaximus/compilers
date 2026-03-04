package compiler.Parser

import scala.util.{Try, Success, Failure}

import compiler.Tokenizer.{Token, TokenType}

sealed abstract class Expression:
  override def toString() = s"Expression(TODO)"

// Return value of statement without return value
case class Unit() {
  override def toString: String = "Unit()"
}

case class Literal(value: Int | Unit) extends Expression {
  override def toString: String = s"Literal($value)"
}
case class Identifier(name: String) extends Expression {
  override def toString: String = s"Identifier($name)"
}
case class BinaryOperator(left: Expression, operator: String, right: Expression) extends Expression {
  override def toString: String = s"BinaryOperator(l: $left, op: $operator, r: $right)"
}
case class UnaryOperator(operator: String, expr: Expression) extends Expression {
  override def toString: String = s"UnaryOperator($operator: $expr)"
}
case class IfThenElse(condition: Expression, body: Expression, elseBody: Option[Expression]) extends Expression {
  override def toString: String = s"If($condition then $body, else $elseBody)"
}
case class WhileDo(condition: Expression, body: Expression) extends Expression {
  override def toString: String = s"While($condition do $body)"
}
case class Declaration(name: Identifier, body: Expression) extends Expression {
  override def toString: String = s"Var($name = $body)"
}
case class Function(name: Identifier, arguments: List[Expression]) extends Expression {
  override def toString: String = s"Function($name(${arguments.mkString(", ")}))"
}
case class Block(expressions: List[Expression]) extends Expression {
  override def toString: String = s"Block({\n${expressions.mkString(";\n")}\n})"
}
case class Other() extends Expression {
  override def toString: String = s"Other()"
}

class Parser(tokens: List[Token]):
  // Keeps track of current token, index to tokens: List[Token]
  private var pos = 0
  // leftAssociativeBinaryOperators
  private val operators = List(
    List("or"),
    List("and"),
    List("==", "!="),
    List("<", "<=", ">", ">="),
    List("+", "-"),
    List("*", "/", "%"),
  )


  def peek(): Token =
    return if pos < tokens.length then tokens(pos) else Token("", TokenType.End, tokens.last.location)

  def previous(): Token =
    return if pos-1 < 0 then tokens(0) else tokens(pos-1)

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


  def parseUnaryOperator(): Expression =
    // Throw error if consume returns Failure
    val operator = consume(Some(List("not", "-"))).get.value
    val expr = parseFactor()
    return UnaryOperator(operator, expr)


  def parseFactor(): Expression =
    val token = peek()
    return token.tokenType match {
      case TokenType.IntLiteral => parseIntLiteral()
      case TokenType.Identifier if token.value == "if" => parseIfThenElse()
      case TokenType.Identifier if token.value == "while" => parseWhileDo()
      case TokenType.Identifier if token.value == "not" => parseUnaryOperator()
      case TokenType.Identifier => parseIdentifier()
      case TokenType.Operator if token.value == "-" => parseUnaryOperator()
      case TokenType.Punctuation if token.value == "(" => parseParenthisized()
      case TokenType.Punctuation if token.value == "{" => parseBlock()
      case _ => throw new Exception(s"Incorrect token type: Expected (, int literal or identifier, got ${token.tokenType} token")
    }


  def parseIfThenElse(): Expression =
    consume(Some("if")).get
    val condition = parseExpression()
    consume(Some("then")).get
    val body = parseExpression()
    var elseBody: Option[Expression] = None
    if peek().value == "else" then
      consume(Some("else"))
      elseBody = Some(parseExpression())

    return IfThenElse(condition, body, elseBody)


  def parseWhileDo(): Expression =
    consume(Some("while")).get
    val condition = parseExpression()
    consume(Some("do")).get
    val body = parseExpression()
    return WhileDo(condition, body)


  def parseFunction(parsedFnName: Option[Identifier] = None): Expression =
    val fnName = parsedFnName.getOrElse(Identifier(consume(Some(TokenType.Identifier)).get.value))
    consume(Some("(")).get
    var args: List[Expression] = List()
    while
      peek().value != ")"
    do
      args = args ::: List(parseExpression())
      if peek().value == "," then consume(Some(",")).get

    consume(Some(")")).get
    return Function(fnName, args)


  def parseEquals(parsedEqualsLeft: Option[Expression], initialDepth: Int = 0): Expression =
    var depth = initialDepth
    val left = parsedEqualsLeft.getOrElse(parseFactor())
    consume(Some("=")).get
    var right = BinaryOperator(left, "=", parseExpression())

    while
      peek().value == "="
    do
      consume(Some("=")).get
      right = BinaryOperator(left, "=", right)
    
    return right


  def parseDeclaration(): Expression =
    val name = parseIdentifier()
    consume(Some("=")).get
    val body = parseExpression(topLevelOrBlock=true)
    return Declaration(name, body)


  def parseBlock(): Expression =
    consume(Some("{")).get
    var expressions: List[Expression] = List()
    var endsInSemicolon: Boolean = false

    while
      peek().value != "}"
    do
      endsInSemicolon = false
      expressions = expressions ::: List(parseExpression(topLevelOrBlock=true))

      if peek().value != "}" && previous().value != "}" && previous().value != ";" then
        consume(Some(";")).get
        endsInSemicolon = true

      if previous().value == "}" then
        if peek().value == ";" then consume(Some(";")).get

    if endsInSemicolon then
      expressions = expressions ::: List(Literal(Unit()))

    consume(Some("}")).get
    // Get rid of hanging semicolons if true then { *something* }; <- !
    if peek().value == ";" then consume(Some(";")).get
    return Block(expressions)

  def parseExpression(initialDepth: Int = 0, topLevelOrBlock: Boolean = false): Expression =
    var depth = initialDepth
    var left = parseFactor()

    left = left match {
      // If identifier is followed by ( => it is a function call (or syntax error :D)
      case left: Identifier if peek().value == "(" => parseFunction(Some(Identifier(left.name)))
      // Allow declarations in only top level or blocks and fail if found elsewhere
      case left: Identifier if left.name == "var" && topLevelOrBlock => parseDeclaration()
      case left: Identifier if left.name == "var" && !topLevelOrBlock => throw new Exception("Declaration found in not toplevel or block")
      case left if peek().value == "=" => parseEquals(Some(left))
      case _ => left
    }
    
    while
      depth < operators.length
    do
      while
        operators(depth).contains(peek().value)
      do
        val operatorToken = consume(Some(operators(depth))).get
        val right = parseExpression(depth + 1)
        left = BinaryOperator(left, operatorToken.value, right)
        depth = initialDepth
      depth += 1

    return left
   

object Parser:
  def parse(tokens: List[Token]): Try[Expression] =
    val parser = Parser(tokens)
    val result = parser.parseExpression()
    return Success(result)
