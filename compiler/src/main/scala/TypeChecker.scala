package compiler.TypeChecker

import compiler.Parser._


abstract class CType()

case class CInt() extends CType
case class CBool() extends CType
case class CUnit() extends CType
case class CNone() extends CType

class CSymbol(val name: Identifier, val value: Expression, val valType: CType) {
  override def equals(other: Any): Boolean = other match {
    case that: CSymbol => if (that.name.name == this.name.name && that.value == this.value && that.valType == this.valType) then true else false
    case _ => false
  }
  override def toString: String =
    s"CSymbol($name, $value, $valType)"
}


class SymbolTable(var symbols: List[CSymbol], var childTable: Option[SymbolTable]) {
  override def equals(other: Any): Boolean = other match {
    case that: SymbolTable => if that.symbols.zip(this.symbols).forall((thatSymbol, thisSymbol) => thatSymbol == thisSymbol) then true else false
    case _ => false
  }
}


class TypeChecker() {
  private var symTab = SymbolTable(List(), None)

  def getSymTable(): SymbolTable =
    return symTab

  def typecheck(ast: Expression): CType =
    return ast match {
      case that: Literal => {
        that.value match {
          case _: Int => CInt()
          case _: Unit => CUnit()
        }
      }

      case that: BinaryOperator => {
        val leftType = typecheck(that.left)
        val rightType = typecheck(that.right)

        return that.operator match {
          case "+" | "-" | "*" | "%" | "/" => {
            (leftType, rightType) match {
              case (_: CInt, _: CInt) => CInt()
              case _ => throw new Exception(s"Incorrect left or right value in BinaryOperator l: $leftType r: $rightType, expected CInt") 
            }
          }
        }
      }
      case that: IfThenElse => {
        typecheck(that.condition) match {
          case _: CBool => {}
          case conditionType => throw new Exception(s"IfThenElse condition not bool: $conditionType")
        }

        val ifBranchType = typecheck(that.body)
        val elseBranchType = that.elseBody match {
          case None => CNone()
          case Some(body) => typecheck(body)
        }

        // TODO: var x = if x>1 then 1 could have type CNone()
        if elseBranchType == CNone() then
          return ifBranchType

        if ifBranchType != elseBranchType then
          throw new Exception(s"IfThenElse branches don't share a type: $ifBranchType vs $elseBranchType")

        return ifBranchType
      }

      case that: Identifier => {
        return that.name match {
          case "true" | "false" => CBool()
          case _ => throw new Exception("NOT IMPLEMENTED")
        }
      }


      case that: WhileDo => {
        // NOTE: Should While do return only CUNit?
        typecheck(that.condition) match {
          case _: CBool => CUnit()
          case conditionType => throw new Exception(s"WhileDo condition not bool: $conditionType")
        }
      }

      case that: Declaration => {
        symTab.symbols ::: List(new CSymbol(that.name, that.body, typecheck(that.body)))
        CUnit() // var x = 1 shouldn't have a type (x can and will have a type)
      }

      case that: Block => {
        // that.expressions.map(expr => typecheck(expr))
        return CUnit()
    }

      // case _ => throw new Exception("NOT IMPLEMENTED")
    }
}


object TypeChecker:
  def typecheck(ast: Expression): CType =
    val ts = TypeChecker()
    ts.typecheck(ast)
