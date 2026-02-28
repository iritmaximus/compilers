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
  // TODO: Fix error message checking
  test("should fail with missing operator".fail) {
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
  // TODO: Fix error message checking
  test("should fail with incorrect operator".fail) {
    val token = Tokenizer.tokenize("1 while").get(1)
    val p = testParser("1 while 2")

    interceptMessage[java.lang.Exception](s"Token $token was not expected: List(+, -)") {
      p.parseExpression()
    }
  }
  // TODO: Commented out, will be implemented later on
  test("should fail with extra int".fail) {
    val p = testParser("1 + 2 3")

    interceptMessage[java.lang.Exception]("Tokens remaining when there should not be") {
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

  // TERM (there is no parseTerm anymore but oh well)
  test("should parse either identifier or int literal (identifier)") {
    val p = testParser("parse_int")
    val result = p.parseExpression()
    val expected = Identifier("parse_int")

    assertEquals(result, expected)
  }
  test("should parse either identifier or int literal (int_literal)") {
    val p = testParser("51080")
    val result = p.parseExpression()
    val expected = Literal(51080)

    assertEquals(result, expected)
  }
  test("should parse either identifier or int literal (both)") {
    val p = testParser("parse_int 14580")
    val r1 = p.parseExpression()
    val r2 = p.parseExpression()
    val expected = List(Identifier("parse_int"), Literal(14580))

    assertEquals(List(r1, r2), expected)
  }

  test("should parse multiple operators with correct associativity") {
    val p = testParser("1 + 2 - 40")
    val result = p.parseExpression()
    val expected = BinaryOperator(BinaryOperator(Literal(1), "+", Literal(2)), "-", Literal(40))

    assertEquals(result, expected)
  }
  test("should parse multiple operators with different precedence levels (* first)") {
    val p = testParser("1 * 2 + 40")
    val result = p.parseExpression()
    val expected = BinaryOperator(BinaryOperator(Literal(1), "*", Literal(2)), "+", Literal(40))

    assertEquals(result, expected)
  }
  test("should parse multiple operators with different precedence levels (* last)") {
    val p = testParser("1 + 2 * 40")
    val result = p.parseExpression()
    val expected = BinaryOperator(Literal(1), "+", BinaryOperator(Literal(2), "*", Literal(40)))

    assertEquals(result, expected)
  }
  test("should parse multiple operators with different precedence levels (* in the middle)") {
    // (1 + (2 * 40)) - 1
    val p = testParser("1 + 2 * 40 - 1")
    val result = p.parseExpression()
    val expected = BinaryOperator(
      BinaryOperator(
        Literal(1),
        "+",
        BinaryOperator(
          Literal(2),
          "*",
          Literal(40))),
      "-",
      Literal(1))

    assertEquals(result, expected)
  }

  // IfThenElse
  test("should parse simple if then expression") {
    val p = testParser("if true then 1 + 1")
    val result = p.parseIfThenElse()
    val expected = IfThenElse(Identifier("true"), BinaryOperator(Literal(1), "+", Literal(1)), None)

    assertEquals(result, expected)
  }
  test("should parse simple if then else expression") {
    val p = testParser("if true then 1 + 1 else 3")
    val result = p.parseIfThenElse()
    val expected = IfThenElse(Identifier("true"), BinaryOperator(Literal(1), "+", Literal(1)), Some(Literal(3)))

    assertEquals(result, expected)
  }
  test("should parse if then else expression with parenthesis") {
    val p = testParser("if (1 + 4) then (1 + 1) else 3")
    val result = p.parseIfThenElse()
    val expected = IfThenElse(BinaryOperator(Literal(1), "+", Literal(4)), BinaryOperator(Literal(1), "+", Literal(1)), Some(Literal(3)))

    assertEquals(result, expected)
  }
  test("should parse if then else expression inside other expressions") {
    val p = testParser("1 + if true then 2 else 3")
    val result = p.parseExpression()
    val expected = BinaryOperator(Literal(1), "+", IfThenElse(Identifier("true"), Literal(2), Some(Literal(3))))

    assertEquals(result, expected)
  }
  test("should not parse if then with extra identifiers") {
    val token = Tokenizer.tokenize("if true while").get(2)
    val p = testParser("if true while then 1 + 1")

    interceptMessage[java.lang.Exception](s"Token $token was not expected: then") {
      p.parseIfThenElse()
    }
  }


  // Function
  test("should parse simple function without parameters") {
    val p = testParser("print_int()")
    val result = p.parseFunction()
    val expected = Function("print_int", List())

    assertEquals(result, expected)
  }
  test("should parse simple function with single parameter") {
    val p = testParser("print_int(1)")
    val result = p.parseFunction()
    val expected = Function("print_int", List(Literal(1)))

    assertEquals(result, expected)
  }
  test("should parse function with multiple parameters") {
    val p = testParser("print_int(1, a, BigInt)")
    val result = p.parseFunction()
    val expected = Function("print_int", List(Literal(1), Identifier("a"), Identifier("BigInt")))

    assertEquals(result, expected)
  }
  test("should parse function with multiple complex parameters") {
    val p = testParser("print_int(x+y, 1 * (xy+z), parse_int)")
    val result = p.parseFunction()
    val expected = Function("print_int", List(
      BinaryOperator(Identifier("x"), "+", Identifier("y")),
      BinaryOperator(Literal(1), "*", BinaryOperator(Identifier("xy"), "+", Identifier("z"))),
      Identifier("parse_int")
    ))

    assertEquals(result, expected)
  }
  test("should parse function with multiple complex parameters + incl. functions") {
    val p = testParser("print_int(x+y, 1 * (xy+z), parse_int())")
    val result = p.parseFunction()
    val expected = Function("print_int", List(
      BinaryOperator(Identifier("x"), "+", Identifier("y")),
      BinaryOperator(Literal(1), "*", BinaryOperator(Identifier("xy"), "+", Identifier("z"))),
      Function("parse_int", List())
    ))

    assertEquals(result, expected)
  }

  // REMAINDER
  test("should parse simple % remainder operator") {
    val p = testParser("5 % 2")
    val result = p.parseExpression()
    val expected = BinaryOperator(Literal(5), "%", Literal(2))

    assertEquals(result, expected)
  }
  test("should parse % remainder with other operators") {
    // (1 * (20 % 4)) + 3
    val p = testParser("1 * 20 % 4 + 3")
    val result = p.parseExpression()
    val expected = BinaryOperator(BinaryOperator(BinaryOperator(Literal(1), "*", Literal(20)), "%", Literal(4)), "+", Literal(3))

    assertEquals(result, expected)
  }

  // COMPARISONS
  test("should parse less than (or equals) comparison with simple expressions") {
    val p1 = testParser("1 + 2 * 40 < parse_int")
    val p2 = testParser("1 + 2 * 40 <= parse_int")
    val r1 = p1.parseExpression()
    val r2 = p2.parseExpression()
    val exp1 = BinaryOperator(
      BinaryOperator(Literal(1), "+", BinaryOperator(Literal(2), "*", Literal(40))),
      "<",
      Identifier("parse_int")
    )
    val exp2 = BinaryOperator(
      BinaryOperator(Literal(1), "+", BinaryOperator(Literal(2), "*", Literal(40))),
      "<=",
      Identifier("parse_int")
    )

    assertEquals(r1, exp1)
    assertEquals(r2, exp2)
  }
  test("should parse greater than (or equals) comparison with simple expressions") {
    val p = testParser("1 + 2 * 40 >= parse_int")
    val result = p.parseExpression()
    val expected = BinaryOperator(
      BinaryOperator(Literal(1), "+", BinaryOperator(Literal(2), "*", Literal(40))),
      ">=",
      Identifier("parse_int")
    )

    assertEquals(result, expected)
    val p1 = testParser("1 + 2 * 40 > parse_int")
    val p2 = testParser("1 + 2 * 40 >= parse_int")
    val r1 = p1.parseExpression()
    val r2 = p2.parseExpression()
    val exp1 = BinaryOperator(
      BinaryOperator(Literal(1), "+", BinaryOperator(Literal(2), "*", Literal(40))),
      ">",
      Identifier("parse_int")
    )
    val exp2 = BinaryOperator(
      BinaryOperator(Literal(1), "+", BinaryOperator(Literal(2), "*", Literal(40))),
      ">=",
      Identifier("parse_int")
    )

    assertEquals(r1, exp1)
    assertEquals(r2, exp2)
  }
  test("should parse simple equal comparison") {
    val p = testParser("20 == 20")
    val result = p.parseExpression()
    val expected = BinaryOperator(Literal(20), "==", Literal(20))

    assertEquals(result, expected)
  }
  test("should parse equal comparison with simple expressions (with parenthesis)") {
    val p = testParser("(1 + 2 * 40) == parse_int")
    val result = p.parseExpression()
    val expected = BinaryOperator(
      BinaryOperator(Literal(1), "+", BinaryOperator(Literal(2), "*", Literal(40))),
      "==",
      Identifier("parse_int")
    )

    assertEquals(result, expected)
  }
  test("should parse equal comparison with simple expressions (without parenthesis)") {
    val p = testParser("1 + 2 * 40 == parse_int")
    val result = p.parseExpression()
    val expected = BinaryOperator(
      BinaryOperator(Literal(1), "+", BinaryOperator(Literal(2), "*", Literal(40))),
      "==",
      Identifier("parse_int")
    )

    assertEquals(result, expected)
  }
  test("should parse equal comparison with simple expressions and functions") {
    val p = testParser("1 + 2 * 40 == parse_int(2)")
    val result = p.parseExpression()
    val expected = BinaryOperator(
      BinaryOperator(Literal(1), "+", BinaryOperator(Literal(2), "*", Literal(40))),
      "==",
      Function("parse_int", List(Literal(2)))
    )

    assertEquals(result, expected)
  }
  test("should parse not-equal comparison with lower precedence comparisons") {
    val p = testParser("1 + 2 * 40 <= parse_int(2) != true")
    val result = p.parseExpression()
    val expected = BinaryOperator(
      BinaryOperator(
        BinaryOperator(Literal(1), "+", BinaryOperator(Literal(2), "*", Literal(40))),
        "<=",
        Function("parse_int", List(Literal(2)))
      ),
      "!=",
      Identifier("true")
    )
    assertEquals(result, expected)
  }
  test("should parse simple comparisons with `and` operator") {
    val p = testParser("true and false")
    val result = p.parseExpression()
    val expected = BinaryOperator(
      Identifier("true"),
      "and",
      Identifier("false")
    )

    assertEquals(result, expected)
  }
  test("should parse comparisons with `and` and lower precedence operators") {
    val p = testParser("true and x == y + 1")
    val result = p.parseExpression()
    val expected = BinaryOperator(
      Identifier("true"),
      "and",
      BinaryOperator(Identifier("x"), "==", BinaryOperator(Identifier("y"), "+", Literal(1)))
    )

    assertEquals(result, expected)
  }
  test("should parse simple comparisons with `or` operator") {
    val p = testParser("true or true")
    val result = p.parseExpression()
    val expected = BinaryOperator(
      Identifier("true"),
      "or",
      Identifier("true")
    )

    assertEquals(result, expected)
  }
  test("should parse comparisons with `or` and lower precedence operators") {
    val p = testParser("true or is_true(x) == (y and 1)")
    val result = p.parseExpression()
    val expected = BinaryOperator(
      Identifier("true"),
      "or",
      BinaryOperator(
        Function("is_true", List(Identifier("x"))),
        "==",
        BinaryOperator(Identifier("y"), "and", Literal(1))
      )
    )

    assertEquals(result, expected)
  }
  test("should parse comparisons with `or` and lower precedence operators (without params)") {
    val p = testParser("true or is_true(x) == y and 1")
    val result = p.parseExpression()
    val expected = BinaryOperator(
      Identifier("true"),
      "or",
      BinaryOperator(
        BinaryOperator(
          Function("is_true", List(Identifier("x"))),
          "==",
          Identifier("y")
        ),
        "and",
        Literal(1)
      )
    )

    assertEquals(result, expected)
  }

  // UNARY OPERATOR
  test("should parse simple unary `not` operator") {
    val p = testParser("not true")
    val result = p.parseExpression()
    val expected = UnaryOperator("not", Identifier("true"))

    assertEquals(result, expected)
  }
  test("should parse simple unary `-` operator") {
    val p = testParser("-true")
    val result = p.parseExpression()
    val expected = UnaryOperator("-", Identifier("true"))

    assertEquals(result, expected)
  }
  test("should parse unary `not` operator in an expression") {
    val p = testParser("if not true then print_int(x)")
    val result = p.parseExpression()
    val expected = IfThenElse(
      UnaryOperator("not", Identifier("true")),
      Function("print_int", List(Identifier("x"))),
      None
    )

    assertEquals(result, expected)
  }
  test("should parse unary `-` operator in an expression") {
    val p = testParser("if - true then print_int(x)")
    val result = p.parseExpression()
    val expected = IfThenElse(
      UnaryOperator("-", Identifier("true")),
      Function("print_int", List(Identifier("x"))),
      None
    )

    assertEquals(result, expected)
  }
}
