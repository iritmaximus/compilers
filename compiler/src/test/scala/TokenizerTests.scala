// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html

import compiler.Tokenizer._
import compiler.Tokenizer.TokenType._
import scala.util.{Try, Failure, Success}


class TokenizerTestWithoutLocation extends munit.FunSuite {
  val L = TokenLocationDebug()

  def test_tokenizer(code: String, expected: List[Token]): Unit =
    val result = Tokenizer.tokenize(code)
    result match {
      case Success(result) => assertEquals(result, expected)
      case Failure(result) => fail("Tokenizing failed: " + result)
    }


  test("test test succeeds") {
    val obtained = 42
    val expected = 42
    assertEquals(obtained, expected)
  }

  test("should work with identifiers") {
    val code = "while true do print_int BigInt"
    val expected = List(
      Token("while", Identifier, L),
      Token("true", Identifier, L),
      Token("do", Identifier, L),
      Token("print_int", Identifier, L),
      Token("BigInt", Identifier, L),
    )
    test_tokenizer(code, expected)
  }

  test("should work with simple operators and int literals") {
    val code = "41 + 3 == 44"
    val expected =
      List(
        Token("41", IntLiteral, L),
        Token("+", Operator, L),
        Token("3", IntLiteral, L),
        Token("==", Operator, L),
        Token("44", IntLiteral, L)
      )
    test_tokenizer(code, expected)
  }

  test("should work with operators and int literals") {
    val code = "1531 + 1241 == 2772"
    val expected =
      List(
        Token("1531", IntLiteral, L), 
        Token("+", Operator, L),
        Token("1241", IntLiteral, L),
        Token("==", Operator, L),
        Token("2772", IntLiteral, L)
      )
    test_tokenizer(code, expected)
  }

  test("should work with comments") {
    val code1 = "4/2==2 #math"
    val code2 = "4/2==2 // math"
    val code3 = "# this is a whole line comment"
    val expected = List(
      Token("4", IntLiteral, L),
      Token("/", Operator, L),
      Token("2", IntLiteral, L),
      Token("==", Operator, L),
      Token("2", IntLiteral, L),
    )
    test_tokenizer(code1, expected)
    test_tokenizer(code2, expected)
    test_tokenizer(code3, Nil)
  }

  test("should work with whole-line comments") {
    val code = "# this is a whole line comment"
    val expected = Nil
    test_tokenizer(code, expected)
  }


  test("should work with function parenthesis") {
    val code = "print_int(n);"
    val expected = List(
      Token("print_int", Identifier, L),
      Token("(", Punctuation, L),
      Token("n", Identifier, L),
      Token(")", Punctuation, L),
      Token(";", Punctuation, L),
    )
    test_tokenizer(code, expected)
  }

  test("should work with functions with types") {
    val code = "test(i: Int,j: Float);"
    val expected = List(
      Token("test", Identifier, L),
      Token("(", Punctuation, L),
      Token("i", Identifier, L),
      Token(":", Punctuation, L),
      Token("Int", Identifier, L),
      Token(",", Punctuation, L),
      Token("j", Identifier, L),
      Token(":", Punctuation, L),
      Token("Float", Identifier, L),
      Token(")", Punctuation, L),
      Token(";", Punctuation, L),
    )
    test_tokenizer(code, expected)
  }

  test("should work with more complicated control flow") {
    val code = "if (i>0) { it(); }"
    val expected = List(
      Token("if", Identifier, L),
      Token("(", Punctuation, L),
      Token("i", Identifier, L),
      Token(">", Operator, L),
      Token("0", IntLiteral, L),
      Token(")", Punctuation, L),
      Token("{", Punctuation, L),
      Token("it", Identifier, L),
      Token("(", Punctuation, L),
      Token(")", Punctuation, L),
      Token(";", Punctuation, L),
      Token("}", Punctuation, L),
    )
    test_tokenizer(code, expected)
  }
}


class TokenizerTestWithLocation extends munit.FunSuite {
  def test_tokenizer(code: String, expected: List[Token]): Unit =
    val result = Tokenizer.tokenize(code)
    result match {
      case Success(result) => assertEquals(result, expected)
      case Failure(result) => fail("Tokenizing failed: " + result)
    }

  test("should fail with incorrect positions") {
    val code = "1453"
    val location = TokenLocation(Position(2, 1), Position(4,10))
    val expected = List(Token(code, IntLiteral, location))
    val result = Tokenizer.tokenize(code)
    result match {
      case Success(result) => assertNotEquals(result, expected)
      case Failure(result) => fail("Tokenizing failed: " + result)
    }
  }

  test("should work with int literals") {
    val code = "1453"
    val location = TokenLocation(Position(1, 1), Position(1,5))
    val expected = List(Token(code, IntLiteral, location))
    test_tokenizer(code, expected)
  }

  test("should work with a newline") {
    val code = "if  3\nwhile"
    val expected = List(
      Token("if", Identifier, TokenLocation(Position(1,1), Position(1,3))),
      Token("3", IntLiteral, TokenLocation(Position(1,5), Position(1,6))),
      Token("while", Identifier, TokenLocation(Position(2,1), Position(2,6)))
    )
    test_tokenizer(code, expected)
  }

  test("should work with multiple newline") {
    val code = "if  3\nwhile\n\nprint_int"
    val expected = List(
      Token("if", Identifier, TokenLocation(Position(1,1), Position(1,3))),
      Token("3", IntLiteral, TokenLocation(Position(1,5), Position(1,6))),
      Token("while", Identifier, TokenLocation(Position(2,1), Position(2,6))),
      Token("print_int", Identifier, TokenLocation(Position(4,1), Position(4,10)))
    )
    test_tokenizer(code, expected)
  }

  test("should work with multiple tokens") {
    val code = "1 + 2 % 4 / 3 * 3 <= 100"
    val expected = List(
      Token("1", IntLiteral, TokenLocation(Position(1,1), Position(1,2))),
      Token("+", Operator, TokenLocation(Position(1,3), Position(1,4))),
      Token("2", IntLiteral, TokenLocation(Position(1,5), Position(1,6))),
      Token("%", Operator, TokenLocation(Position(1,7), Position(1,8))),
      Token("4", IntLiteral, TokenLocation(Position(1,9), Position(1,10))),
      Token("/", Operator, TokenLocation(Position(1,11), Position(1,12))),
      Token("3", IntLiteral, TokenLocation(Position(1,13), Position(1,14))),
      Token("*", Operator, TokenLocation(Position(1,15), Position(1,16))),
      Token("3", IntLiteral, TokenLocation(Position(1,17), Position(1,18))),
      Token("<=", Operator, TokenLocation(Position(1,19), Position(1,21))),
      Token("100", IntLiteral, TokenLocation(Position(1,22), Position(1,25)))
    )
    test_tokenizer(code, expected)
  }
}
