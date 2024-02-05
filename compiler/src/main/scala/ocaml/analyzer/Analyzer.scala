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

  def lookupTypeStr(value: String) = {
    value match {
      case "int"   => Types.Integer()
      case "float" => Types.Float()
      case _       => Types.Unknown("'" + value)
    }
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
        ValEntry(tree.identifier.value, lookupType(tree.identifier))
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
            ValEntry(tree.identifier.value, value._type)
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

  def evaluate(tree: Trees.Tuple, enviroment: Enviroment): T = {
    tree.entries.forall((entry) => {
      evaluate(entry, enviroment) match {
        case None    => false
        case Some(_) => true
      }
    })

    val _type = Types.Tuple(
      tree.entries.map((entry) => {
        evaluate(entry, enviroment).get._type
      }),
      List()
    );

    Some(
      AnalyzerResult(
        _type,
        enviroment
      )
    )
  }

  def evaluate(tree: Trees.Constructor, enviroment: Enviroment): T = {
    evaluate(tree.values, enviroment) match {
      case Some(res) => {
        println("Type")
        println(res._type)
        println(findType(tree, enviroment))
        findType(tree, enviroment) match {
          case Some(TypeEntry(_, _type: Types.Constraint)) if _type.of.get == res._type => {
            return Some(AnalyzerResult(_type, enviroment))
          }
          case _ => None
        }
      }
      case None => None
    }

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
      case a: Trees.Tuple           => evaluate(a, enviroment)
      case a: Trees.Constructor     => evaluate(a, enviroment)
      case a: Trees.If              => evaluate(a, enviroment)
      case a: Trees.LetBindingExpr  => evaluate(a, enviroment)
      case a: Trees.OperatorExpr    => evaluate(a, enviroment)
      case a: Trees.Identifier      => evaluate(a, enviroment)
      case a: Trees.Record          => evaluate(a, enviroment)
      case a: Trees.Assignment      => evaluate(a, enviroment)
      case a: Trees.TypeDeclaration => evaluate(a, enviroment)
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

  def evaluate(tree: Trees.TypeDeclaration, enviroment: Enviroment): T = {
    def getType(_type: Trees.TypeExp, name: Option[String]): Option[Types.Type] = {
      _type match {
        case value: Trees.TypeTuple =>
          Some(
            Types.Tuple(
              value.value.map((v) => getType(v, None).get),
              List()
            )
          )
        case value: Trees.TypeFieldDeclarations =>
          Some(
            Types.Record(
              name.getOrElse(""),
              value.definitions.map(v => Types.RecordField(v.key, getType(v._type, None).get)),
              List()
            )
          )
        case value: Trees.TypeEquation => getType(value.definitions, None)
        case value: Trees.TypeConstraints =>
          Some(
            Types.Constraints(
              name.get,
              value.definitions.map(v => {
                v.of match {
                  case Some(value) => Types.Constraint(v.name, getType(value, None), List())
                  case None        => Types.Constraint(v.name, None, List())
                }
              }),
              List()
            )
          )

        case Trees.TypePrimitive(key) if key == "float" => Some(Types.Float())
        case Trees.TypePrimitive(key) if key == "int"   => Some(Types.Integer())
        case _                                          => None
      }
    }

    tree.information match {
      case Trees.TypeEquation(_type) =>
        Some(
          AnalyzerResult(
            Types.Unit(),
            enviroment.addType(
              TypeEntry(tree.identifier, getType(tree.information, Some(tree.identifier)).get)
            )
          )
        )
      case _type: Trees.TypeFieldDeclarations =>
        Some(
          AnalyzerResult(
            Types.Unit(),
            enviroment.addType(
              TypeEntry(tree.identifier, getType(_type, Some(tree.identifier)).get)
            )
          )
        )
      case _type: Trees.TypeConstraints =>
        Some(
          AnalyzerResult(
            Types.Unit(),
            enviroment.addType(
              TypeEntry(tree.identifier, getType(_type, Some(tree.identifier)).get)
            )
          )
        )

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

  def findType(tree: Trees.Constructor, enviroment: Enviroment): Option[TypeEntry] = {
    val types = enviroment.entries.filter(v => v.isInstanceOf[TypeEntry]).asInstanceOf[List[TypeEntry]]
    types.reverse.foreach(_type => {
      _type._type match {
        case constr: Types.Constraints => {
          constr.constraints.find(p => p.name == tree.identifier) match {
            case Some(value) => return Some(TypeEntry(value.name, value))
            case None        =>
          }
        }
      }
    })
    None
  }

  def findType(tree: Trees.Record, enviroment: Enviroment): Option[TypeEntry] = {
    val types = enviroment.entries.filter(v => v.isInstanceOf[TypeEntry]).asInstanceOf[List[TypeEntry]]
    types.reverse.map(_type => {
      _type._type match {
        case Types.Record(_, fields, generics) if fields.length == tree.entries.length => {
          if (
            fields.forall((b) => {
              val entry = tree.entries.find(v => v.key == b.key);
              entry match {
                case Some(v) => {
                  evaluate(v.value, enviroment).get._type == b._type
                }
                case None => false
              }
            })
          )
            return Some(_type)
        }
      }
    })

    None
  }

  def evaluate(tree: Trees.Record, enviroment: Enviroment): T = {
    println(tree)
    findType(tree, enviroment) match {
      case Some(value) => Some(AnalyzerResult(value._type, enviroment))
      case None        => None
    }
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
                enviroment.copyWith(ValEntry(tree.variable.value, out._type))
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
