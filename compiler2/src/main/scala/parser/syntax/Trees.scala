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

  case class Identifier(value: String) extends Expr
  case class TypedIdentifier(value: String) extends Expr

  object Tokens {
    case class IntLit(value: Integer) extends Expr
    case class StringLit(value: String) extends Expr
    case class FloatLit(value: Float) extends Expr
    case class OpenParantheses() extends Tree
    case class CloseParantheses() extends Tree

    case class Plus() extends Operator
    case class Minus() extends Operator
    case class Multiply() extends Operator
    case class Divide() extends Operator
    case class Equal() extends Operator

    case class Let() extends Keyword

  }

  case class OperatorExpr(
      left: Expr,
      right: Expr,
      operator: Operator
  ) extends Expr

  case class LetBinding(
      identifiers: List[Identifier],
      expression: Expr
  ) extends Expr

  case class Assignment(
      variable: Identifier,
      operator: Expr
  ) extends Expr

  case class Parentensis(
      content: Expr
  ) extends Expr

  case class Empty(
  ) extends Expr

  case class Program(statements: List[Tree]) extends Tree

}
