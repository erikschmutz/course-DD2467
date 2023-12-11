import ocaml.trees._
import ocaml.types._
import ocaml.enviroment._

case class AnalyzerResult(_type: Types.Type, enviroment: Enviroment) {}

object Analyzer {
  type T = Option[AnalyzerResult]

  def evaluate(tree: Trees.Tree, enviroment: Enviroment): T = {
    tree match {
      case a: Trees.Tokens.IntLit => evaluate(a, enviroment)
      case a: Trees.Assignment    => evaluate(a, enviroment)
      case a: Trees.OperatorExpr  => evaluate(a, enviroment)
      case a: Trees.Identifier    => evaluate(a, enviroment)
      case _                      => None
    }
  }

  def evaluate(identifier: Trees.Identifier, enviroment: Enviroment): T = {
    enviroment.lookup(identifier) match {
      case Some(value) => Some(AnalyzerResult(value._type, enviroment))
      case None        => None
    }
  }

  def evaluate(tree: Trees.Tokens.IntLit, enviroment: Enviroment): T = {
    Some(
      AnalyzerResult(Types.Integer(), enviroment)
    )
  }

  def evaluate(tree: Trees.Assignment, enviroment: Enviroment): T = {
    evaluate(tree.exprs, enviroment) match {
      case Some(value) =>
        Some(
          AnalyzerResult(
            Types.Integer(),
            enviroment.copyWith(
              EnviromentEntry(tree.variable, value._type)
            )
          )
        )
      case None => None
    }
  }

  def evaluate(tree: Trees.OperatorExpr, enviroment: Enviroment): T = {
    val left = evaluate(tree.left, enviroment)
    val right = evaluate(tree.right, enviroment)

    println("left")
    println(left)

    (left, tree.operator, right) match {
      case (
            Some(AnalyzerResult(l: Types.Integer, _)),
            op: Trees.Tokens.Plus,
            Some(AnalyzerResult(r: Types.Integer, _))
          ) =>
        Some(AnalyzerResult(Types.Integer(), enviroment))
      case _ => None
    }
  }

  def evaluate(trees: List[Trees.Tree], enviroment: Enviroment): T = {
    trees match {
      case head :: Nil => {
        evaluate(head, enviroment)
      }
      case head :: rest => {
        evaluate(head, enviroment) match {
          case Some(a) => evaluate(rest, a.enviroment)
          case None    => None
        }
      }
      case Nil => None
    }
  }

  def evaluate(prog: Trees.Program): T = {
    val result = evaluate(prog.statements, Enviroment(List()))
    print(result)
    result
  }
}
