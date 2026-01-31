// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html

import compiler.Tokenizer.*

class TokenizerTest extends munit.FunSuite {
  test("test test succeeds") {
    val obtained = 42
    val expected = 42
    assertEquals(obtained, expected)
  }

  test("should work with int literals") {
    val code = "1453"
    val expected = List(Token(code))
    val result = tokenizer(code)
    assertEquals(result.toString(), expected.toString())
  }

  test("should work with simple operatorn and int literals") {
    val code = "41 + 3 == 44"
    val expected =
      List(Token("41"), Token("+"), Token("3"), Token("=="), Token("44"))
    val result = tokenizer(code)
    assertEquals(result.toString(), expected.toString())
  }

  test("should work with operators and int literals") {
    val code = "1531 + 1241 == 2772"
    val expected =
      List(Token("1531"), Token("+"), Token("1241"), Token("=="), Token("2772"))
    val result = tokenizer(code)
    assertEquals(result.toString(), expected.toString())
  }
}
