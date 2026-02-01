// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html

import compiler.Tokenizer.*
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
      IdentifierT("while", L),
      IdentifierT("true", L),
      IdentifierT("do", L),
      IdentifierT("print_int", L),
      IdentifierT("BigInt", L),
    )
    test_tokenizer(code, expected)
  }

  test("should work with simple operators and int literals") {
    val code = "41 + 3 == 44"
    val expected =
      List(
        IntLiteralT("41", L),
        OperatorT("+", L),
        IntLiteralT("3", L),
        OperatorT("==", L),
        IntLiteralT("44", L)
      )
    test_tokenizer(code, expected)
  }

  test("should work with operators and int literals") {
    val code = "1531 + 1241 == 2772"
    val expected =
      List(IntLiteralT("1531", L), OperatorT("+", L), IntLiteralT("1241", L), OperatorT("==", L), IntLiteralT("2772", L))
    test_tokenizer(code, expected)
  }

  test("should work with comments") {
    val code1 = "4/2==2 #math"
    val code2 = "4/2==2 // math"
    val code3 = "# this is a whole line comment"
    val expected = List(
      IntLiteralT("4", L),
      OperatorT("/", L),
      IntLiteralT("2", L),
      OperatorT("==", L),
      IntLiteralT("2", L),
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
      IdentifierT("print_int", L),
      PunctuationT("(", L),
      IdentifierT("n", L),
      PunctuationT(")", L),
      PunctuationT(";", L),
    )
    test_tokenizer(code, expected)
  }

  test("should work with functions with types") {
    val code = "test(i: Int,j: Float);"
    val expected = List(
      IdentifierT("test", L),
      PunctuationT("(", L),
      IdentifierT("i", L),
      PunctuationT(":", L),
      IdentifierT("Int", L),
      PunctuationT(",", L),
      IdentifierT("j", L),
      PunctuationT(":", L),
      IdentifierT("Float", L),
      PunctuationT(")", L),
      PunctuationT(";", L),
    )
    test_tokenizer(code, expected)
  }

  test("should work with more complicated control flow") {
    val code = "if (i>0) { it(); }"
    val expected = List(
      IdentifierT("if", L),
      PunctuationT("(", L),
      IdentifierT("i", L),
      OperatorT(">", L),
      IntLiteralT("0", L),
      PunctuationT(")", L),
      PunctuationT("{", L),
      IdentifierT("it", L),
      PunctuationT("(", L),
      PunctuationT(")", L),
      PunctuationT(";", L),
      PunctuationT("}", L),
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
    val expected = List(IntLiteralT(code, location))
    val result = Tokenizer.tokenize(code)
    result match {
      case Success(result) => assertNotEquals(result, expected)
      case Failure(result) => fail("Tokenizing failed: " + result)
    }
  }

  test("should work with int literals") {
    val code = "1453"
    val location = TokenLocation(Position(1, 1), Position(1,5))
    val expected = List(IntLiteralT(code, location))
    test_tokenizer(code, expected)
  }

  test("should work with a newline") {
    val code = "if  3\nwhile"
    val expected = List(
      IdentifierT("if", TokenLocation(Position(1,1), Position(1,3))),
      IntLiteralT("3", TokenLocation(Position(1,5), Position(1,6))),
      IdentifierT("while", TokenLocation(Position(2,1), Position(2,6)))
    )
    test_tokenizer(code, expected)
  }

  test("should work with multiple newline") {
    val code = "if  3\nwhile\n\nprint_int"
    val expected = List(
      IdentifierT("if", TokenLocation(Position(1,1), Position(1,3))),
      IntLiteralT("3", TokenLocation(Position(1,5), Position(1,6))),
      IdentifierT("while", TokenLocation(Position(2,1), Position(2,6))),
      IdentifierT("print_int", TokenLocation(Position(4,1), Position(4,10)))
    )
    test_tokenizer(code, expected)
  }

  test("should work with multiple tokens") {
    val code = "1 + 2 % 4 / 3 * 3 <= 100"
    val expected = List(
      IntLiteralT("1", TokenLocation(Position(1,1), Position(1,2))),
      OperatorT("+", TokenLocation(Position(1,3), Position(1,4))),
      IntLiteralT("2", TokenLocation(Position(1,5), Position(1,6))),
      OperatorT("%", TokenLocation(Position(1,7), Position(1,8))),
      IntLiteralT("4", TokenLocation(Position(1,9), Position(1,10))),
      OperatorT("/", TokenLocation(Position(1,11), Position(1,12))),
      IntLiteralT("3", TokenLocation(Position(1,13), Position(1,14))),
      OperatorT("*", TokenLocation(Position(1,15), Position(1,16))),
      IntLiteralT("3", TokenLocation(Position(1,17), Position(1,18))),
      OperatorT("<=", TokenLocation(Position(1,19), Position(1,21))),
      IntLiteralT("100", TokenLocation(Position(1,22), Position(1,25)))
    )
    test_tokenizer(code, expected)
  }
}
