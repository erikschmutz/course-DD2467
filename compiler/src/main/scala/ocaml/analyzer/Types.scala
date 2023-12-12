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
  case class Unit() extends Type {
    def prettyPrint: String = "()"
  }
  case class Function(input: List[Type], output: Type) extends Type {
    def prettyPrint: String = {
      if (input.length < 2)
        return input.map(arg => arg.prettyPrint).mkString(",") + "=>" + output.prettyPrint

      "(" + input.map(arg => arg.prettyPrint).mkString(",") + ")" + "=>" + output.prettyPrint
    }

  }
}
