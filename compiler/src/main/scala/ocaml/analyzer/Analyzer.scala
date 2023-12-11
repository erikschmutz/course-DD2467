import ocaml.trees._
import ocaml.types._
import ocaml.trees.Trees.Operator
import java.util.Optional

case class AnalyzerResult(ok: Boolean, _type: String) {}

object Analyzer {
  type T = Option[Types.Type]

  def evaluate(tree: Trees.Tree): T = {
    tree match {
      case a: Trees.OperatorExpr     => evaluate(a)
      case a: Trees.Tokens.IntLit    => evaluate(a)
      case a: Trees.Tokens.StringLit => evaluate(a)
      case a: Trees.Assignment       => evaluate(a)
      case _                         => None
    }
  }

  def evaluate(tree: Trees.Tokens.StringLit): T = {
    Option(Types.String())
  }

  def evaluate(tree: Trees.Assignment): T = {
    evaluate(tree.exprs)
  }

  def evaluate(tree: Trees.Tokens.IntLit): T = {
    Option(Types.Integer())
  }

  def evaluate(tree: Trees.OperatorExpr): T = {
    val left = evaluate(tree.left)
    val right = evaluate(tree.right)

    (left, tree.operator, right) match {
      case (Some(a: Types.Integer), b: Trees.Tokens.Plus, Some(c: Types.Integer)) => Some(Types.Integer())
      case _                                                                      => None
    }
  }

  def evaluate(trees: List[Trees.Tree]): Option[Types.Type] = {
    trees match {
      case head :: Nil => {
        println("Head")
        println(head)
        evaluate(head)
      }
      case head :: rest => {
        evaluate(head) match {
          case Some(a) => evaluate(rest)
          case None    => None
        }
      }
      case Nil => None
    }
  }

  def evaluate(prog: Trees.Program): Option[Types.Type] = {
    evaluate(prog.statements)
  }
}
