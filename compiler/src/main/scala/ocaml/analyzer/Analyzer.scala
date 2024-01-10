import ocaml.trees._
import ocaml.types._
import ocaml.enviroment._

case class AnalyzerResult(_type: Types.Type, enviroment: Enviroment) extends PrettyPrint {
  override def toString() = {
    "AnalyzerResult(" + _type + "," + enviroment + ")"
  }

  def prettyPrint: String = {
    enviroment.prettyPrint + "\n- :" + _type.prettyPrint
  }
}

object Analyzer {
  type T = Option[AnalyzerResult]

  def canBeType[Q](value: Any) = {
    value.isInstanceOf[Q] || value.isInstanceOf[Types.Unknown]
  }

  def lookupType(identifier: Trees.Identifier) = {
    identifier.varibleType match {
      case Some(v) if v == "int"   => Types.Integer()
      case Some(v) if v == "float" => Types.Float()
      case _                       => Types.Unknown("'" + identifier.value)
    }
  }

  def evaluate(tree: Trees.LetBinding, enviroment: Enviroment): T = {
    log(tree, enviroment)

    evaluate(
      tree.expression,
      enviroment.copyWith(
        EnviromentEntry(tree.identifier.value, lookupType(tree.identifier))
      )
    ) match {
      case Some(value) => {
        val _type = value.enviroment.lookup(tree.identifier.value).get._type
        Some(AnalyzerResult(Types.Function(_type, value._type), value.enviroment.withType(tree.identifier, _type)))
      }
      case None => {
        None
      }
    }
  }

  def evaluate(tree: Trees.LetBindingExpr, enviroment: Enviroment): T = {
    log(tree, enviroment)

    evaluate(tree.value, enviroment) match {
      case Some(value) =>
        evaluate(
          tree.expr,
          value.enviroment.copyWith(
            EnviromentEntry(tree.identifier.value, value._type)
          )
        ) match {
          case Some(value) => {

            Some(AnalyzerResult(value._type, enviroment))
          }
          case _ => None
        }
      case _ => None
    }

  }

  def evaluate(tree: Trees.Recursive, enviroment: Enviroment): T = {
    log(tree, enviroment)
    Some(AnalyzerResult(Types.Function(Types.Unknown("a"), Types.Unknown("b")), enviroment))
  }

  def evaluate(tree: Trees.Tree, enviroment: Enviroment): T = {
    tree match {
      case a: Trees.Tokens.IntLit   => evaluate(a, enviroment)
      case a: Trees.Tokens.CharLit  => evaluate(a, enviroment)
      case a: Trees.Tokens.Bool     => evaluate(a, enviroment)
      case a: Trees.Recursive       => evaluate(a, enviroment)
      case a: Trees.Tokens.FloatLit => evaluate(a, enviroment)
      case a: Trees.Substitutions   => evaluate(a, enviroment)
      case a: Trees.LetBinding      => evaluate(a, enviroment)
      case a: Trees.If              => evaluate(a, enviroment)
      case a: Trees.LetBindingExpr  => evaluate(a, enviroment)
      case a: Trees.OperatorExpr    => evaluate(a, enviroment)
      case a: Trees.Identifier      => evaluate(a, enviroment)
      case a: Trees.Assignment      => evaluate(a, enviroment)
      case _                        => None
    }
  }

  def log(tree: Trees.Tree, enviroment: Enviroment): Unit = {
    // println(tree)
    // println(enviroment.prettyPrint)
  }

  def evaluate(tree: Trees.OperatorExpr, enviroment: Enviroment): T = {
    log(tree, enviroment)
    val left = evaluate(tree.left, enviroment)
    val right = evaluate(tree.right, enviroment)

    (left, tree.operator, right) match {
      case (
            Some(AnalyzerResult(l, le)),
            op: Trees.Tokens.Plus,
            Some(AnalyzerResult(r, re))
          ) if canBeType[Types.Integer](l) && canBeType[Types.Integer](r) =>
        Some(
          AnalyzerResult(
            Types.Integer(),
            enviroment
              .copyWith(le.entries)
              .copyWith(re.entries)
              .withType(tree.left, Types.Integer())
              .withType(tree.right, Types.Integer())
          )
        )
      case (
            Some(AnalyzerResult(l, le)),
            op: Trees.Tokens.Minus,
            Some(AnalyzerResult(r, re))
          ) if canBeType[Types.Integer](l) && canBeType[Types.Integer](r) =>
        Some(
          AnalyzerResult(
            Types.Integer(),
            enviroment
              .copyWith(le.entries)
              .copyWith(re.entries)
              .withType(tree.left, Types.Integer())
              .withType(tree.right, Types.Integer())
          )
        )
      case (
            Some(AnalyzerResult(l, le)),
            op: Trees.Tokens.FloatPlus,
            Some(AnalyzerResult(r, re))
          ) if canBeType[Types.Float](l) && canBeType[Types.Float](r) =>
        Some(
          AnalyzerResult(
            Types.Float(),
            enviroment
              .copyWith(le.entries)
              .copyWith(re.entries)
              .withType(tree.left, Types.Float())
              .withType(tree.right, Types.Float())
          )
        )
      case (
            Some(AnalyzerResult(l, le)),
            op: Trees.Tokens.GreaterThan,
            Some(AnalyzerResult(r, re))
          ) if canBeType[Types.Integer](l) && canBeType[Types.Integer](r) =>
        Some(
          AnalyzerResult(
            Types.Bool(),
            enviroment
              .copyWith(le.entries)
              .copyWith(re.entries)
              .withType(tree.left, Types.Integer())
              .withType(tree.right, Types.Integer())
          )
        )

      case (
            Some(AnalyzerResult(l, le)),
            op: Trees.Tokens.LessThan,
            Some(AnalyzerResult(r, re))
          ) if canBeType[Types.Integer](l) && canBeType[Types.Integer](r) =>
        Some(
          AnalyzerResult(
            Types.Bool(),
            enviroment
              .copyWith(le.entries)
              .copyWith(re.entries)
              .withType(tree.left, Types.Integer())
              .withType(tree.right, Types.Integer())
          )
        )

      case (
            Some(AnalyzerResult(l, le)),
            op: Trees.Tokens.Multiply,
            Some(AnalyzerResult(r, re))
          ) if canBeType[Types.Integer](l) && canBeType[Types.Integer](r) =>
        Some(
          AnalyzerResult(
            Types.Integer(),
            enviroment
              .copyWith(le.entries)
              .copyWith(re.entries)
              .withType(tree.left, Types.Integer())
              .withType(tree.right, Types.Integer())
          )
        )
      case _ => {

        None
      }
    }
  }

  def evaluate(tree: Trees.Tokens.IntLit, enviroment: Enviroment): T = {
    Some(
      AnalyzerResult(Types.Integer(), enviroment)
    )
  }

  def evaluate(tree: Trees.Tokens.CharLit, enviroment: Enviroment): T = {
    Some(
      AnalyzerResult(Types.Char(), enviroment)
    )
  }

  def evaluate(tree: Trees.Tokens.Bool, enviroment: Enviroment): T = {
    Some(
      AnalyzerResult(Types.Bool(), enviroment)
    )
  }

  def evaluate(tree: Trees.Tokens.FloatLit, enviroment: Enviroment): T = {
    Some(
      AnalyzerResult(Types.Float(), enviroment)
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

          case Some(out) => {
            Some(
              AnalyzerResult(
                out._type,
                enviroment.copyWith(EnviromentEntry(tree.variable.value, out._type))
              )
            )
          }
          case _ => {

            None
          }
        }

      }
      case _ => {

        None
      }
    }
  }

  def evaluate(tree: Trees.If, enviroment: Enviroment): T = {
    log(tree, enviroment)
    val result = evaluate(tree.expr, enviroment)

    evaluate(tree.expr, enviroment) match {
      case Some(AnalyzerResult(_: Types.Bool, enviroment)) => {

        val thn = evaluate(tree.thn, enviroment)
        val els = evaluate(tree.els, enviroment)

        (thn, els) match {
          case (
                Some(AnalyzerResult(thnType, le)),
                Some(AnalyzerResult(elsType, re))
              ) if elsType == thnType => {
            return Some(AnalyzerResult(thnType, le.copyWith(re.entries)))
          }

          case (
                Some(AnalyzerResult(thnType, le)),
                Some(AnalyzerResult(elsType, re))
              ) if thnType.isInstanceOf[Types.Unknown] && !elsType.isInstanceOf[Types.Unknown] => {
            return Some(AnalyzerResult(elsType, le.copyWith(re.entries)))
          }

          case (
                Some(AnalyzerResult(thnType, le)),
                Some(AnalyzerResult(elsType, re))
              ) if !thnType.isInstanceOf[Types.Unknown] && elsType.isInstanceOf[Types.Unknown] => {
            return Some(AnalyzerResult(thnType, le.copyWith(re.entries)))
          }

          case _ => {

            None
          }
        }
      }

      case _ => {

        None
      }
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
    log(tree, enviroment)

    evaluate(tree.value_name, enviroment) match {
      case Some(AnalyzerResult(func: Types.Function, enviroment)) => {
        evaluate(tree.values, enviroment) match {
          case Some(arg) if arg._type == func.input => {
            return Some(AnalyzerResult(func.output, enviroment))
          }

          case Some(arg) if func.input.isInstanceOf[Types.Unknown] && func.output.isInstanceOf[Types.Unknown] => {
            if (func.input.asInstanceOf[Types.Unknown].id == func.output.asInstanceOf[Types.Unknown].id)
              return Some(AnalyzerResult(arg._type, enviroment))
            else
              return Some(AnalyzerResult(func.output, enviroment))
          }

          case Some(arg) if func.input.isInstanceOf[Types.Unknown] => {
            return Some(AnalyzerResult(func.output, enviroment))
          }

          case Some(arg) if arg._type.isInstanceOf[Types.Unknown] && !func.input.isInstanceOf[Types.Unknown] => {
            return Some(AnalyzerResult(func.output, enviroment.withType(tree.values, func.input)))
          }

          case Some(arg) => {
            println("failed2", arg)
            return None
          }
          case _ => {

            None
          }
        }

      }
      case _ => {
        print("Failed sub...", tree.value_name, enviroment)
        return None
      }
    }

  }
}
