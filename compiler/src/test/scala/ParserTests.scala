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

    result match {
      case Success(that) => assertEquals(that, expected)
      case Failure(that) => fail("Parsing failed: " + that)
    }
  }
  test("should parse bigger ints correctly") {
    val p = testParser("59819")
    val result = p.parseIntLiteral()
    val expected = Literal(59819)

    result match {
      case Success(that) => assertEquals(that, expected)
      case Failure(that) => fail("Parsing failed: " + that)
    }
  }
  test("should parse separate ints correctly") {
    val p = testParser("5 1 40")
    val result1 = p.parseIntLiteral()
    val result2 = p.parseIntLiteral()
    val result3 = p.parseIntLiteral()
    val expected = List(Literal(5), Literal(1), Literal(40))

    (result1, result2, result3) match {
      case (Success(r1), Success(r2), Success(r3)) => assertEquals(List(r1, r2, r3), expected)
      case _ => fail("Parsing failed")
    }
  }
  test("should not parse non-int values") {
    val p = testParser("else")
    val result = p.parseIntLiteral()

    result match {
      case Success(that) => fail("Parsing should have failed but succeeded: " + that)
      case Failure(that) => assert(true)
    }
  }

  // BINAY OPERATOR TESTS
  test("should parse simple sum") {
    val p = testParser("1 + 2")
    val result = p.parseExpression()
    val expected = BinaryOperator(Literal(1), "+", Literal(2))

    result match {
      case Success(that) => assertEquals(that, expected)
      case Failure(that) => fail("Parsing failed: " + that)
    }
  }
  test("should parse simple sum with identifiers") {
    val p = testParser("1 + x")
    val result = p.parseExpression()
    val expected = BinaryOperator(Literal(1), "+", Identifier("x"))

    result match {
      case Success(that) => assertEquals(that, expected)
      case Failure(that) => fail("Parsing failed: " + that)
    }
  }
  test("should parse simple sum with only identifiers") {
    val p = testParser("value + tokenLen")
    val result = p.parseExpression()
    val expected = BinaryOperator(Identifier("value"), "+", Identifier("tokenLen"))

    result match {
      case Success(that) => assertEquals(that, expected)
      case Failure(that) => fail("Parsing failed: " + that)
    }
  }
  test("should fail with missing operator") {
    val p = testParser("1 2")
    val result = p.parseExpression()

    result match {
      case Success(that) => fail("Parsing should have failed but succeeded: " + that)
      case Failure(that) => assert(true)
    }
  }
  test("should fail with missing ints") {
    val p1 = testParser("1 +")
    val result1 = p1.parseExpression()

    val p2 = testParser("+ 2")
    val result2 = p2.parseExpression()

    (result1, result2) match {
      case (Failure(r1), Failure(r2)) => assert(true)
      case _ => fail("Parsing should have failed but succeeded" + result1 + result2)
    }
  }
  test("should fail with incorrect operator") {
    val p = testParser("1 while 2")
    val result = p.parseExpression()

    result match {
      case Success(that) => fail("Parsing should have failed but succeeded: " + that)
      case Failure(that) => assert(true)
    }
  }

  // IDENTIFIER
  test("should parse simple identifier") {
    val p = testParser("parse_int")
    val result = p.parseIdentifier()
    val expected = Identifier("parse_int")

    result match {
      case Success(that) => assertEquals(that, expected)
      case Failure(that) => fail("Parsing failed: " + that)
    }
  }
  test("should parse multiple simple identifiers in a row") {
    val p = testParser("parse_int BigInt")
    val result1 = p.parseIdentifier()
    val result2 = p.parseIdentifier()
    val expected = List(Identifier("parse_int"), Identifier("BigInt"))

    (result1, result2) match {
      case (Success(r1), Success(r2)) => assertEquals(List(r1, r2), expected)
      case (r1, r2) => fail(s"Parsing failed: ${r1}, ${r2}")
    }
  }

  // TERM
  test("should parse either identifier or int literal (identifier)") {
    val p = testParser("parse_int")
    val result = p.parseTerm()
    val expected = Identifier("parse_int")

    result match {
      case Success(that) => assertEquals(that, expected)
      case Failure(that) => fail("Parsing failed: " + that)
    }
  }
  test("should parse either identifier or int literal (int_literal)") {
    val p = testParser("51080")
    val result = p.parseTerm()
    val expected = Literal(51080)

    result match {
      case Success(that) => assertEquals(that, expected)
      case Failure(that) => fail("Parsing failed: " + that)
    }
  }
  test("should parse either identifier or int literal (both)") {
    val p = testParser("parse_int 14580")
    val result1 = p.parseTerm()
    val result2 = p.parseTerm()
    val expected = List(Identifier("parse_int"), Literal(14580))

    (result1, result2) match {
      case (Success(r1), Success(r2)) => assertEquals(List(r1, r2), expected)
      case (r1, r2) => fail(s"Parsing failed: ${r1}, ${r2}")
    }
  }


  test("should parse multiple operators") {
    val p = testParser("1 + 2 - 40")
    val result = p.parseExpression()
    val expected = BinaryOperator(BinaryOperator(Literal(1), "+", Literal(2)), "-", Literal(40))

    result match {
      case Success(that) => assertEquals(that, expected)
      case Failure(that) => fail("Parsing failed: " + that)
    }
  }

}
