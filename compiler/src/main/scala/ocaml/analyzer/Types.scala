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
  case class Float() extends Type {
    def prettyPrint: String = "float"
  }
  case class Unit() extends Type {
    def prettyPrint: String = "()"
  }
  case class Function(input: Type, output: Type) extends Type {
    def prettyPrint: String = {
      input.prettyPrint + "=>" + output.prettyPrint
    }

  }
}
