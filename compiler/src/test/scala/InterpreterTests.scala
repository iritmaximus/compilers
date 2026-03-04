import compiler.Tokenizer._
import compiler.Tokenizer.TokenType as tt
import compiler.Parser._
import compiler.Interpreter._
import scala.util.{Try, Failure, Success}

abstract class BaseInterpreterTests extends munit.FunSuite {
  def getTokens(code: String): Option[List[Token]] =
    val tokens = Tokenizer.tokenize(code)
    return tokens match {
      case Success(tokens) => Some(tokens)
      case Failure(tokens) => fail("Tokenizing failed: " + tokens)
    }

  def testParser(code: String): Parser =
    val tokens = getTokens(code)
    return Parser(tokens.getOrElse(List(Token("error", tt.Error, TokenLocationDebug()))))

  def testInterpreter(code: String) =
    val parser = testParser(code)
    val ast = parser.parseExpression()
    Interpreter.interpret(ast)
}

class InterpreterLiteralTests extends BaseInterpreterTests {
  test("should interpret 1 correctly") {
    val result = testInterpreter("1").get
    val expected = 1

    assertEquals(result, expected)
  }
}
