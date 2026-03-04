import scala.util.{Try, Failure, Success}

import compiler.Tokenizer._
import compiler.Tokenizer.TokenType as tt
import compiler.Parser._
import compiler.TypeChecker._

abstract class BaseTypeCheckerTests extends munit.FunSuite {
  def getTokens(code: String): Option[List[Token]] =
    val tokens = Tokenizer.tokenize(code)
    return tokens match {
      case Success(tokens) => Some(tokens)
      case Failure(tokens) => fail("Tokenizing failed: " + tokens)
    }
  def testParser(code: String): Parser =
    val tokens = getTokens(code)
    return Parser(tokens.getOrElse(List(Token("error", tt.Error, TokenLocationDebug()))))

  def testTypeChecker(code: String): CType =
    val tokens = getTokens(code)
    val ast = Parser(tokens.getOrElse(List(Token("error", tt.Error, TokenLocationDebug())))).parseExpression()
    val typechecker = TypeChecker()
    return typechecker.typecheck(ast)
    
}

class TypeCheckerLiteralTests extends BaseTypeCheckerTests {
  test("should typecheck IntLiteral correctly") {
    List("1", "45910", "0").map(value => {
      val result = testTypeChecker(value)
      val expected = CInt()
      assertEquals(result, expected)
    })
  }
  test("should typecheck booleans") {
    List("true", "false").map(value => {
      val result = testTypeChecker(value)
      val expected = CBool()
      assertEquals(result, expected)
    })
  }
  // TODO
  test("should typecheck negative IntLiteral".fail) {
    val result = testTypeChecker("-14")
    val expected = CInt()
    assertEquals(result, expected)
  }
}

class TypeCheckerBinaryOperatorTests extends BaseTypeCheckerTests {
  test("should typecheck BinaryOperator with simple operators") {
    List("1 + 1", "1580 + 0", "594 - 594", "5 * 5", "47 / 3", "2 % 5").map(value => {
      val result = testTypeChecker(value)
      val expected = CInt()
      assertEquals(result, expected)
    })
  }
}

class TypeCheckerIfThenElseTests extends BaseTypeCheckerTests {
  test("should typecheck simple IfThenElse with bodies as IntLiterals") {
    val result = testTypeChecker("if true then 1 else 2")
    val expected = CInt()
    assertEquals(result, expected)
  }
  test("should typecheck simple IfThenElse with if-branch as IntLiteral and else-branch None") {
    val result = testTypeChecker("if true then 1")
    val expected = CInt()
    assertEquals(result, expected)
  }
  test("should fail typecheck fi IfThenElse branches have different types") {
    interceptMessage[java.lang.Exception]("IfThenElse branches don't share a type: CInt() vs CBool()") {
      testTypeChecker("if true then 1 else false")
    }
  }
}

class TypeCheckerWhileDoTests extends BaseTypeCheckerTests {
  test("should typecheck WhileDo with correct condition") {
    val result = testTypeChecker("while true do 1")
    val expected = CUnit()
    assertEquals(result, expected)
  }
  test("should not typecheck WhileDo with incorrect condition") {
    interceptMessage[java.lang.Exception]("WhileDo condition not bool: CInt()") {
      testTypeChecker("while 4 do true")
    }
  }
}
