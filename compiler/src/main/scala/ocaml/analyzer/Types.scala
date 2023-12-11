package ocaml.types

object Types {
  abstract class Type
  case class Unknown() extends Type
  case class Integer() extends Type
  case class String() extends Type
  case class Function() extends Type
}
