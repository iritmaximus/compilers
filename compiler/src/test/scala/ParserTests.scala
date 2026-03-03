import compiler.Tokenizer._
import compiler.Tokenizer.TokenType as tt
import compiler.Parser._
import scala.util.{Try, Failure, Success}

abstract class BaseParserTests extends munit.FunSuite {
  def getTokens(code: String): Option[List[Token]] =
    val tokens = Tokenizer.tokenize(code)
    return tokens match {
      case Success(tokens) => Some(tokens)
      case Failure(tokens) => fail("Tokenizing failed: " + tokens)
    }

  def testParser(code: String): Parser =
    val tokens = getTokens(code)
    return Parser(tokens.getOrElse(List(Token("error", tt.Error, TokenLocationDebug()))))
}


class ParserIntLiteralTests extends BaseParserTests {
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
}


class ParserBinaryOperatorTests extends BaseParserTests {
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
}


class ParserIdentifierTests extends BaseParserTests {
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
}


class ParserTermTests extends BaseParserTests {
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
}


class ParserAssociativityTests extends BaseParserTests {
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
}


class ParserIfThenElseTests extends BaseParserTests {
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
  test("should parse nested if else") {
    val p = testParser("if a then b else if c then d else e")
    val result = p.parseExpression()
    val expected = IfThenElse(
      Identifier("a"),
      Identifier("b"),
      Some(IfThenElse(
        Identifier("c"),
        Identifier("d"),
        Some(Identifier("e"))
      ))
    )

    assertEquals(result, expected)
  }
  test("should not parse if then with extra identifiers") {
    val token = Tokenizer.tokenize("if true while").get(2)
    val p = testParser("if true while then 1 + 1")

    interceptMessage[java.lang.Exception](s"Token $token was not expected: then") {
      p.parseIfThenElse()
    }
  }
}


class ParserFunctionTests extends BaseParserTests {
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
}


class ParserRemainderTests extends BaseParserTests {
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
}


class ParserPrecedenceTests extends BaseParserTests {
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
}


class ParserComparisonTests extends BaseParserTests {
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

}


class ParserUnaryOperatorTests extends BaseParserTests {
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
  

class ParserEqualTests extends BaseParserTests {
  test("should parse simple assignment") {
    val p = testParser("a = b")
    val result = p.parseExpression()
    val expected = BinaryOperator(Identifier("a"), "=", Identifier("b"))

    assertEquals(result, expected)
  }
  test("should parse simple triple assignment right-associatively") {
    val p = testParser("a = b = c")
    val result = p.parseExpression()
    val expected = BinaryOperator(
      Identifier("a"),
      "=",
      BinaryOperator(Identifier("b"), "=", Identifier("c"))
    )

    assertEquals(result, expected)
  }
  test("should parse triple assignment with expressions right-associatively") {
    val p = testParser("a = b = 1+print_bool(not true)")
    val result = p.parseExpression()
    val expected = BinaryOperator(
      Identifier("a"),
      "=",
      BinaryOperator(
        Identifier("b"),
        "=",
        BinaryOperator(Literal(1), "+", Function("print_bool", List(UnaryOperator("not", Identifier("true")))))
      )
    )

    assertEquals(result, expected)
  }
}


class ParseWhileDoTests extends BaseParserTests {
  test("should parse simple while do") {
    val p = testParser("while true do print()")
    val result = p.parseExpression()
    val expected = WhileDo(Identifier("true"), Function("print", List()))

    assertEquals(result, expected)
  }
  test("should parse while do with expressions") {
    val p = testParser("while i+3 <= x do parse_bool(true) != false")
    val result = p.parseExpression()
    val expected = WhileDo(
      BinaryOperator(
        BinaryOperator(Identifier("i"), "+", Literal(3)),
        "<=",
        Identifier("x")
      ),
      BinaryOperator(Function("parse_bool", List(Identifier("true"))), "!=", Identifier("false"))
    )

    assertEquals(result, expected)
  }
}


class ParserDeclarationTests extends BaseParserTests {
  test("should parse simple declaration") {
    val p = testParser("var x = 1")
    val result = p.parseExpression(topLevelOrBlock=true)
    val expected = Declaration(Identifier("x"), Literal(1))

    assertEquals(result, expected)
  }
  test("should parse declaration with expression body") {
    val p = testParser("var result = parse(1+x)")
    val result = p.parseExpression(topLevelOrBlock=true)
    val expected = Declaration(
      Identifier("result"),
      Function("parse", List(BinaryOperator(Literal(1), "+", Identifier("x"))))
     )

    assertEquals(result, expected)
  }

  test("should not parse declaration with non-identifier name") {
    val token = Tokenizer.tokenize("var x>").get(2)
    // var (1+1) is function...
    val p = testParser("var x>y = parse(1+x)")
    interceptMessage[java.lang.Exception](s"Token $token was not expected: =") {
      val result = p.parseExpression(topLevelOrBlock=true)
    }
  }
  test("should not parse declaration with non-identifier name (function)".fail) {
    val token = Tokenizer.tokenize("var (").get(1)
    // var (1+1) is function...
    val p = testParser("var (1+1) = parse(1+x)")
    interceptMessage[java.lang.Exception](s"Token $token was not expected: Identifier") {
      val result = p.parseExpression(topLevelOrBlock=true)
    }
  }
}

class ParserBlockTests extends BaseParserTests {
  test("should parse empty block") {
    val p = testParser("{}")
    val result = p.parseExpression()
    val expected = Block(List())

    assertEquals(result, expected)
  }
  test("should parse simple block with single expression (no semicolon)") {
    val p = testParser("{ x }")
    val result = p.parseExpression()
    val expected = Block(List(Identifier("x")))

    assertEquals(result, expected)
  }
  test("should parse simple block with single expression (semicolon)") {
    val p = testParser("{ x; }")
    val result = p.parseExpression()
    val expected = Block(List(Identifier("x"), Literal(Unit())))

    assertEquals(result, expected)
  }
  test("should parse block with single declaration (no semicolon)") {
    val p = testParser("{ var x = 1 }")
    val result = p.parseExpression()
    val expected = Block(List(Declaration(Identifier("x"), Literal(1))))

    assertEquals(result, expected)
  }
  test("should parse block with sigle declaration (semicolon)") {
    val p = testParser("{ var x = 1; }")
    val result = p.parseExpression()
    val expected = Block(List(Declaration(Identifier("x"), Literal(1)), Literal(Unit())))

    assertEquals(result, expected)
  }
  test("should parse block with multiple declarations") {
    val p = testParser("{ var x = 1; var y = 2; }")
    val result = p.parseExpression()
    val expected = Block(List(
      Declaration(Identifier("x"), Literal(1)),
      Declaration(Identifier("y"), Literal(2)),
      Literal(Unit()
    )))

    assertEquals(result, expected)
  }
  test("should parse with multiple function calls and return value") {
    val p = testParser("{ f(a); x = y; f(x) }")
    val result = p.parseExpression(topLevelOrBlock=true)
    val expected = Block(List(
      Function("f", List(Identifier("a"))),
      BinaryOperator(Identifier("x"), "=", Identifier("y")),
      Function("f", List(Identifier("x")))
    ))

    assertEquals(result, expected)
  }
  test("should parse if then else with blocks") {
    val p = testParser("if true then { var x = 1; print_int(x); } else { var x = 2; }")
    val result = p.parseExpression(topLevelOrBlock=true)
    val expected = IfThenElse(
      Identifier("true"),
      Block(List(
        Declaration(Identifier("x"), Literal(1)),
        Function("print_int", List(Identifier("x"))),
        Literal(Unit())
      )),
      Some(Block(List(
        Declaration(Identifier("x"), Literal(2)),
        Literal(Unit())))
      )
    )

    assertEquals(result, expected)
  }
  test("should parse assignment with block as right side") {
    val p = testParser("x = { f(a); b }")
    val result = p.parseExpression(topLevelOrBlock=true)
    val expected = BinaryOperator(
      Identifier("x"),
      "=",
      Block(List(
        Function("f", List(Identifier("a"))),
        Identifier("b")
      ))
    )
    assertEquals(result, expected)
  }
  test("should allow trailing semicolon after block") {
    val p = testParser("{ f(a); };")
    val result = p.parseExpression(topLevelOrBlock=true)
    val expected = Block(List(
      Function("f", List(Identifier("a"))),
      Literal(Unit())
    ))
      
    assertEquals(result, expected)
  }

  test("should fail to parse if trying to declare variable outside block and not top-level") {
    val token = Tokenizer.tokenize("if true then var").get(3)
    val p = testParser("if true then var x = 1")
    interceptMessage[java.lang.Exception]("Declaration found in not toplevel or block") {
      val result = p.parseExpression(topLevelOrBlock=true)
    }
  }
}

class ParserBlockSemicolonTests extends BaseParserTests {
  test("should parse two blocks inside a block") {
    val p = testParser("{ { a } { b } }")
    val result = p.parseExpression(topLevelOrBlock=true)
    val expected = Block(List(
      Block(List(Identifier("a"))),
      Block(List(Identifier("b")))
    ))

    assertEquals(result, expected)
  }
  test("should parse two blocks inside a block (with semicolons)") {
    val p = testParser("{ { a }; { b } }")
    val result = p.parseExpression(topLevelOrBlock=true)
    val expected = Block(List(
      Block(List(Identifier("a"))),
      Block(List(Identifier("b")))
    ))

    assertEquals(result, expected)
  }
  test("should parse identifier after IfThen block") {
    // { if true then { a } b }
    val p = testParser("{ if true then { a } b }")
    val result = p.parseExpression(topLevelOrBlock=true)
    val expected = Block(List(
      IfThenElse(
        Identifier("true"),
        Block(List(Identifier("a"))),
        None
      ),
      Identifier("b")
    ))

    assertEquals(result, expected)
  }
  test("should parse identifier after IfThen block (with semicolon)") {
    // { if true then { a } b }
    val p = testParser("{ if true then { a }; b }")
    val result = p.parseExpression(topLevelOrBlock=true)
    val expected = Block(List(
      IfThenElse(
        Identifier("true"),
        Block(List(Identifier("a"))),
        None
      ),
      Identifier("b")
    ))

    assertEquals(result, expected)
  }
  test("should not parse identifiers inside block") {
    val token = Tokenizer.tokenize("{ a b }").get(2)
    val p = testParser("{ a b }")
    interceptMessage[java.lang.Exception](s"Token $token was not expected: ;") {
      val result = p.parseExpression(topLevelOrBlock=true)
    }
  }
  test("should not parse two identifiers after block in IfThenElse") {
    // { if true then { a } b c }
    val token = Tokenizer.tokenize("{ if true then { a } b c }").get(8)
    val p = testParser("{ if true then { a } b c }")
    interceptMessage[java.lang.Exception](s"Token $token was not expected: ;") {
      val result = p.parseExpression(topLevelOrBlock=true)
    }
  }
}
