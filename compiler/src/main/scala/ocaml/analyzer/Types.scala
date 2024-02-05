package ocaml.types

abstract class PrettyPrint {
  def prettyPrint: String;
}

object Types {
  abstract class Type extends PrettyPrint {}

  case class Unknown(id: String) extends Type {
    def prettyPrint: String = id

  }
  case class Integer() extends Type {
    def prettyPrint: String = "int"
  }
  case class Double() extends Type {
    def prettyPrint: String = "double"
  }

  case class RecordField(key: String, _type: Type)
  case class Record(name: String, fields: List[RecordField], generics: List[String]) extends Type {
    def prettyPrint: String = name + " {" + fields.map(v => v.key + ":" + v._type.prettyPrint).mkString(",") + "}"
  }

  case class Constraint(name: String, of: Option[Type], generics: List[String]) extends Type {
    def prettyPrint: String = {
     println("Constaint ", of)
      of match {
        case Some(value) => name + " of " + value.prettyPrint
        case None        => name
      }
    }
  }

  case class Constraints(name: String, constraints: List[Constraint], generics: List[String]) extends Type {
    def prettyPrint: String = name + " (" + constraints.map(c => c.prettyPrint).mkString(" | ") + ")"
  }

  case class Tuple(fields: List[Type], generics: List[String]) extends Type {
    def prettyPrint: String = fields.map(_.prettyPrint).mkString(" * ")
  }

  case class ArrayList(_type: Type) extends Type {
    def prettyPrint: String = _type.prettyPrint + "[||]"
  }

  case class Char() extends Type {
    def prettyPrint: String = "char"
  }
  case class Float() extends Type {
    def prettyPrint: String = "float"
  }
  case class Unit() extends Type {
    def prettyPrint: String = "()"
  }
  case class Bool() extends Type {
    def prettyPrint: String = "bool"
  }
  case class Function(input: Type, output: Type) extends Type {
    def prettyPrint: String = {
      input.prettyPrint + "=>" + output.prettyPrint
    }
  }

}
