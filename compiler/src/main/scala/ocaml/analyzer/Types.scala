package ocaml.types

object Types {
  abstract class Type
  case class Integer() extends Type
  case class String() extends Type
}
