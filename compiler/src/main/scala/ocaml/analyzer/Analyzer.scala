import ocaml.trees._
import ocaml.types._
import ocaml.enviroment._

case class AnalyzerResult(_type: Types.Type, enviroment: Enviroment) extends PrettyPrint {
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

  def lookupType(identifier: Trees.Identifier) = {
    identifier.varibleType match {
      case Some(v) if v == "int" => Types.Integer()
      case _                     => Types.Unknown("'" + identifier.value)
    }
  }

  def evaluate(tree: Trees.LetBinding, enviroment: Enviroment): T = {
    evaluate(
      tree.expression,
      enviroment.copyWith(
        EnviromentEntry(tree.identifier.value, lookupType(tree.identifier))
      )
    ) match {
      case Some(value) => {

        val _type = value.enviroment.lookup(tree.identifier.value).get._type
        Some(AnalyzerResult(Types.Function(_type, value._type), value.enviroment))
      }
      case None => None
    }
  }

  def evaluate(tree: Trees.Tree, enviroment: Enviroment): T = {
    tree match {
      case a: Trees.Tokens.IntLit => evaluate(a, enviroment)
      case a: Trees.Substitutions => evaluate(a, enviroment)
      case a: Trees.LetBinding    => evaluate(a, enviroment)
      case a: Trees.OperatorExpr  => evaluate(a, enviroment)
      case a: Trees.Identifier    => evaluate(a, enviroment)
      case a: Trees.Assignment    => evaluate(a, enviroment)
      case _                      => None
    }
  }

  def evaluate(tree: Trees.OperatorExpr, enviroment: Enviroment): T = {
    val left = evaluate(tree.left, enviroment)
    val right = evaluate(tree.right, enviroment)

    (left, tree.operator, right) match {
      case (
            Some(AnalyzerResult(l, _)),
            op: Trees.Tokens.Plus,
            Some(AnalyzerResult(r, _))
          ) if canBeType[Types.Integer](l) && canBeType[Types.Integer](r) =>
        Some(
          AnalyzerResult(
            Types.Integer(),
            enviroment.withType(tree.left, Types.Integer()).withType(tree.right, Types.Integer())
          )
        )
      case (
            Some(AnalyzerResult(l, _)),
            op: Trees.Tokens.Multiply,
            Some(AnalyzerResult(r, _))
          ) if canBeType[Types.Integer](l) && canBeType[Types.Integer](r) =>
        Some(
          AnalyzerResult(
            Types.Integer(),
            enviroment.withType(tree.left, Types.Integer()).withType(tree.right, Types.Integer())
          )
        )
      case _ => None
    }
  }

  def evaluate(tree: Trees.Tokens.IntLit, enviroment: Enviroment): T = {
    Some(
      AnalyzerResult(Types.Integer(), enviroment)
    )
  }

  def evaluate(tree: Trees.Identifier, enviroment: Enviroment): T = {
    enviroment.lookup(tree.value) match {
      case Some(value) => {
        Some(AnalyzerResult(value._type, enviroment))
      }
      case None => None
    }
  }

  def evaluate(tree: Trees.Assignment, enviroment: Enviroment): T = {
    enviroment.lookup(tree.variable.value) match {
      case None => {
        evaluate(tree.exprs, enviroment) match {

          case Some(out) =>
            Some(
              AnalyzerResult(
                out._type,
                enviroment.copyWith(EnviromentEntry(tree.variable.value, out._type))
              )
            )
          case _ => None
        }

      }
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
    result
  }

  def evaluate(tree: Trees.Substitutions, enviroment: Enviroment): T = {
    evaluate(tree.value_name, enviroment) match {
      case Some(AnalyzerResult(func: Types.Function, _)) => {
        evaluate(tree.values, enviroment) match {
          case Some(arg) if arg._type == func.input => {
            return Some(AnalyzerResult(func.output, enviroment))
          }

          case Some(arg) if func.input.isInstanceOf[Types.Unknown] => {

            return Some(AnalyzerResult(arg._type, enviroment))
          }

          case _ => None
        }

      }
      case _ => return None
    }

  }

}
