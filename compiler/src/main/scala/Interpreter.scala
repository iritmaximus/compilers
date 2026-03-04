package compiler.Interpreter

import compiler.Parser._
import compiler.Tokenizer._

type Value = Option[Int | Boolean]

// TODO: Let's do this later...
class Interpreter() {
  def interpret(expr: Expression): Value =
    return expr match {
      case that: BinaryOperator => {
        val left = interpret(that.left).get
        val right = interpret(that.right).get

        that.operator match {
          // case "+" => left + right
          case _ => throw new Exception("NOT IMPLEMENTED")
        }
      }
      case that: Literal => {
        return that.value match {
          case value: Int => Some(value)
          case value: Unit => throw new Exception("NOT IMPLEMENTED")
        }
      }
      case that: IfThenElse => throw new Exception("NOT IMPLEMENTED")
      case _ => throw new Exception(s"Couldn't match expression type, got: $expr")
    }
}

object Interpreter:
  def interpret(ast: Expression): Value =
    val interpreter = Interpreter()
    return interpreter.interpret(ast)
