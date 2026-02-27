import compiler.Tokenizer._
import compiler.Tokenizer.TokenType as tt
import compiler.Parser._
import scala.util.{Try, Failure, Success}


class ParserTests extends munit.FunSuite {
  def getTokens(code: String): Option[List[Token]] =
    val tokens = Tokenizer.tokenize(code)
    return tokens match {
      case Success(tokens) => Some(tokens)
      case Failure(tokens) => fail("Tokenizing failed: " + tokens)
    }

  def testParser(code: String): Parser =
    val tokens = getTokens(code)
    return Parser(tokens.getOrElse(List(Token("error", tt.Error, TokenLocationDebug()))))


  // INT LITERAL TESTS
  test("should parse single ints correctly") {
    val p = testParser("7")
    val result = p.parseIntLiteral()
    val expected = Literal(7)

    assertEquals(result, expected)
  }

  test("should parse bigger ints correctly") {
    val p = testParser("59819")
    val result = p.parseIntLiteral()
    val expected = Literal(59819)

    assertEquals(result, expected)
  }
  test("should parse separate ints correctly") {
    val p = testParser("5 1 40")
    val r1 = p.parseIntLiteral()
    val r2 = p.parseIntLiteral()
    val r3 = p.parseIntLiteral()
    val expected = List(Literal(5), Literal(1), Literal(40))

    assertEquals(List(r1, r2, r3), expected)
  }
  test("should not parse non-int values") {
    val token = Tokenizer.tokenize("else").get(0)
    val p = testParser("else")

    interceptMessage[java.lang.Exception](s"Token $token was not expected: IntLiteral") {
      val result = p.parseIntLiteral()
    }
  }

  // BINAY OPERATOR TESTS
  test("should parse simple sum") {
    val p = testParser("1 + 2")
    val result = p.parseExpression()
    val expected = BinaryOperator(Literal(1), "+", Literal(2))
    assertEquals(result, expected)
  }
  test("should parse simple sum with identifiers") {
    val p = testParser("1 + x")
    val result = p.parseExpression()
    val expected = BinaryOperator(Literal(1), "+", Identifier("x"))

    assertEquals(result, expected)
  }
  test("should parse simple sum with only identifiers") {
    val p = testParser("value + tokenLen")
    val result = p.parseExpression()
    val expected = BinaryOperator(Identifier("value"), "+", Identifier("tokenLen"))

    assertEquals(result, expected)
  }
  test("should fail with missing operator") {
    val errorToken = Tokenizer.tokenize("1 2").get(1)
    val p = testParser("1 2")

    interceptMessage[java.lang.Exception](s"Token $errorToken was not expected: List(+, -)") {
      val result = p.parseExpression()
    }
  }
  test("should fail with missing ints") {
    val p1 = testParser("1 +")
    val p2 = testParser("+ 2")

    interceptMessage[java.lang.Exception](s"Incorrect token type: Expected (, int literal or identifier, got End token") {
      p1.parseExpression()
    }
    interceptMessage[java.lang.Exception](s"Incorrect token type: Expected (, int literal or identifier, got Operator token") {
      p2.parseExpression()
    }
  }
  test("should fail with incorrect operator") {
    val token = Tokenizer.tokenize("1 while").get(1)
    val p = testParser("1 while 2")

    interceptMessage[java.lang.Exception](s"Token $token was not expected: List(+, -)") {
      p.parseExpression()
    }
  }
  test("should fail with extra int") {
    val p = testParser("1 + 2 3")

    interceptMessage[java.lang.Exception]("Tokens left when there should not be") {
      p.parseExpression()
    }
  }
    

  // IDENTIFIER
  test("should parse simple identifier") {
    val p = testParser("parse_int")
    val result = p.parseIdentifier()
    val expected = Identifier("parse_int")
    assertEquals(result, expected)
  }
  test("should parse multiple simple identifiers in a row") {
    val p = testParser("parse_int BigInt")
    val r1 = p.parseIdentifier()
    val r2 = p.parseIdentifier()
    val expected = List(Identifier("parse_int"), Identifier("BigInt"))

    assertEquals(List(r1, r2), expected)
  }

  // TERM
  test("should parse either identifier or int literal (identifier)") {
    val p = testParser("parse_int")
    val result = p.parseTerm()
    val expected = Identifier("parse_int")

    assertEquals(result, expected)
  }
  test("should parse either identifier or int literal (int_literal)") {
    val p = testParser("51080")
    val result = p.parseTerm()
    val expected = Literal(51080)

    assertEquals(result, expected)
  }
  test("should parse either identifier or int literal (both)") {
    val p = testParser("parse_int 14580")
    val r1 = p.parseTerm()
    val r2 = p.parseTerm()
    val expected = List(Identifier("parse_int"), Literal(14580))

    assertEquals(List(r1, r2), expected)
  }

  test("should parse multiple operators") {
    val p = testParser("1 + 2 - 40")
    val result = p.parseExpression()
    val expected = BinaryOperator(BinaryOperator(Literal(1), "+", Literal(2)), "-", Literal(40))

    assertEquals(result, expected)
  }

}
