import ocaml.trees._
import ocaml.types._
import ocaml.enviroment._

case class AnalyzerResult(_type: Types.Type, enviroment: Enviroment, tree: Trees.Tree) extends PrettyPrint {
  override def toString() = {
    "AnalyzerResult(" + _type + "," + enviroment + ")"
  }

  def prettyPrint: String = {
    enviroment.prettyPrint + "\n" + _type.prettyPrint
  }
}

object Analyzer {
  type T = Option[AnalyzerResult]

  def canBeType[Q](value: Any) = {
    value.isInstanceOf[Q] || value.isInstanceOf[Types.Unknown]
  }

  def evaluate(tree: Trees.Tree, enviroment: Enviroment): T = {
    tree match {
      case a: Trees.Tokens.IntLit => evaluate(a, enviroment)
      case a: Trees.Assignment    => evaluate(a, enviroment)
      case a: Trees.OperatorExpr  => evaluate(a, enviroment)
      case a: Trees.Identifier    => evaluate(a, enviroment)
      case a: Trees.Substitutions => evaluate(a, enviroment)
      case a: Trees.LetBinding    => evaluate(a, enviroment)
      case _                      => None
    }
  }

  def evaluate(identifier: Trees.Identifier, enviroment: Enviroment): T = {
    enviroment.lookup(identifier) match {
      case Some(value) => Some(AnalyzerResult(value._type, enviroment, identifier))
      case None        => None
    }
  }

  def evaluate(identifier: Trees.Substitutions, enviroment: Enviroment): T = {
    enviroment.lookup(identifier.value_name) match {
      case Some(value) => {
        val args = identifier.values.map(value => evaluate(value, enviroment).get._type);
        Some(
          AnalyzerResult(
            value._type,
            enviroment.replace(
              identifier.value_name,
              Types.Function(args, value._type)
            ),
            identifier
          )
        )
      }
      case None => None
    }
  }

  def evaluate(tree: Trees.Tokens.IntLit, enviroment: Enviroment): T = {
    Some(
      AnalyzerResult(Types.Integer(), enviroment, tree)
    )
  }

  def evaluate(tree: Trees.Assignment, enviroment: Enviroment): T = {
    evaluate(tree.exprs, enviroment) match {
      case Some(value) => {
        println("Value")

        Some(
          AnalyzerResult(
            Types.Integer(),
            enviroment.copyWith(
              EnviromentEntry(tree.variable, value._type)
            ),
            tree
          )
        )
      }
      case None => None
    }
  }

  def evaluate(tree: Trees.OperatorExpr, enviroment: Enviroment): T = {
    val left = evaluate(tree.left, enviroment)
    val right = evaluate(tree.right, enviroment)

    (left, tree.operator, right) match {
      case (
            Some(AnalyzerResult(l, _, _)),
            op: Trees.Tokens.Plus,
            Some(AnalyzerResult(r, _, _))
          ) if canBeType[Types.Integer](l) && canBeType[Types.Integer](r) =>
        Some(
          AnalyzerResult(
            Types.Integer(),
            enviroment.replace(tree.left, Types.Integer()).replace(tree.right, Types.Integer()),
            tree
          )
        )
      case _ => None
    }
  }

  def evaluate(tree: Trees.LetBinding, enviroment: Enviroment): T = {
    evaluate(
      tree.expression,
      enviroment.copyWith(
        tree.identifiers.map((v) => EnviromentEntry(v, Types.Unknown("'" + v.value)))
      )
    ) match {
      case Some(value) => {
        val types = tree.identifiers.map((v) => value.enviroment.lookup(v).get._type)
        Some(AnalyzerResult(Types.Function(types, value._type), enviroment, tree))
      }
      case None => None
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
    result
  }
}
