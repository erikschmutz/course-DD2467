package ocaml.trees
import ocaml.tokens._
import java.awt.Taskbar.State
import scala.util.parsing.input.Positional

// 1 + 1
object Trees {
  abstract class Tree extends Positional
  abstract class Expr extends Tree
  abstract class Operator extends Tree
  abstract class Keyword extends Tree

  case class Identifier(value: String, varibleType: Option[String]) extends Expr {
    override def toString() = {
      varibleType match {
        case Some(_type) => "Identifier(" + value + "," + _type + ")"
        case None        => "Identifier(" + value + ")"
      }
    }
  }
  case class TypedIdentifier(value: String) extends Expr
  case class ConstIdentifier(value: String) extends Expr

  case class RecordEntry(key: String, value: Expr) extends Expr
  case class Record(entries: List[RecordEntry]) extends Expr
  case class Tuple(entries: List[Expr]) extends Expr
  case class ArrayList(entries: List[Expr]) extends Expr
  case class Constructor(identifier: String, values: Expr) extends Expr

  
  object Tokens {
    case class IntLit(value: Integer) extends Expr
    case class StringLit(value: String) extends Expr
    case class CharLit(value: String) extends Expr
    case class FloatLit(value: Float) extends Expr
    case class DoubleLit(value: Double) extends Expr
    case class TypeParam(value: String) extends Expr
    case class Bool(value: Boolean) extends Expr
    case class Unit() extends Expr

    case class OpenParantheses() extends Tree
    case class CloseParantheses() extends Tree

    case class OpenCurly() extends Tree
    case class CloseCurly() extends Tree
    
    case class ArrayOpen() extends Tree
    case class ArrayClose() extends Tree

    case class Comma() extends Tree
    case class Line() extends Tree
    case class Of() extends Tree

    case class LeftArrow() extends Tree
    case class Function() extends Tree
    case class Divider() extends Tree
    case class Semicolon() extends Tree

    case class FloatPlus() extends Operator
    case class FloatMinus() extends Operator
    case class FloatDivide() extends Operator
    case class FloatMultiply() extends Operator

    case class DoublePlus() extends Operator
    case class DoubleMinus() extends Operator
    case class DoubleMultiply() extends Operator
    case class DoubleDivide() extends Operator

    case class Plus() extends Operator
    case class Minus() extends Operator
    case class Multiply() extends Operator
    case class Divide() extends Operator
    case class Colon() extends Operator
    case class Equal() extends Operator

    case class LessThan() extends Operator
    case class GreaterThan() extends Operator

    case class Let() extends Keyword
    case class In() extends Keyword
    case class If() extends Keyword
    case class Then() extends Keyword
    case class Else() extends Keyword
    case class Rec() extends Keyword
    case class Type() extends Keyword
  }

  case class OperatorExpr(
      left: Expr,
      right: Expr,
      operator: Operator
  ) extends Expr

  case class LetBinding(
      identifier: Trees.Identifier,
      expression: Expr
  ) extends Expr

  case class Recursive(
  ) extends Expr

  case class LetBindingExpr(
      identifier: Trees.Identifier,
      value: Expr,
      expr: Expr
  ) extends Expr

  case class If(
      expr: Expr,
      thn: Expr,
      els: Expr
  ) extends Expr

  case class Substitutions(
      value_name: Tree,
      values: Expr
  ) extends Expr

  case class Assignment(
      variable: Identifier,
      exprs: Expr
  ) extends Expr

  case class Empty(
  ) extends Expr

  abstract class TypeExp extends Tree
  case class TypePrimitive(_type: String) extends TypeExp
  case class TypeTuple(value: List[TypeExp]) extends TypeExp
  case class TypeFunction(input: TypeExp, output: TypeExp) extends TypeExp
  
  case class TypeFieldDeclaration(key: String, _type: TypeExp)
  case class TypeOption(identifier: String) extends TypeExp
  case class TypeConstraint(name: String, of: Option[TypeExp]) extends TypeInformation

  abstract class TypeInformation extends TypeExp
  case class TypeFieldDeclarations(definitions: List[TypeFieldDeclaration]) extends TypeInformation
  case class TypeConstraints(definitions: List[TypeConstraint]) extends TypeInformation
  case class TypeEquation(definitions: TypeExp) extends TypeInformation
  case class TypeUsage(identifier: String, options: List[TypeOption]) extends TypeExp
  case class TypeDeclaration(identifier: String, information: TypeInformation, options: List[TypeOption]) extends Tree

  case class Program(statements: List[Tree]) extends Tree

}
