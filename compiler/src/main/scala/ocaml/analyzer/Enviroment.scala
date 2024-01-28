package ocaml.enviroment

import ocaml.types._
import ocaml.trees.Trees

abstract class EnvEntry(identifier: String, _type: Types.Type) extends PrettyPrint

case class ValEntry(identifier: String, _type: Types.Type) extends EnvEntry(identifier, _type) {
  def prettyPrint: String = "val " + identifier + " : " + _type.prettyPrint

  override def toString = {
    "ValEntry(Identifier(" + identifier + ")," + _type + ")"
  }
}

case class TypeEntry(identifier: String, _type: Types.Type) extends EnvEntry(identifier, _type) {
  def prettyPrint: String = "type " + identifier + " : " + _type.prettyPrint

  override def toString = {
    "TypeEntry(Identifier(" + identifier + ")," + _type + ")"
  }

}

case class Enviroment(entries: List[EnvEntry]) extends PrettyPrint {

  def prettyPrint: String = {
    return entries.map(entry => entry.prettyPrint).mkString("\n")
  }

  def replaceEntries(newEntries: List[EnvEntry], entries: List[EnvEntry]): List[EnvEntry] = {
    val filtered = entries.filter(entry => !newEntries.contains(entry));
    filtered ::: newEntries
  }

  def addType(entry: TypeEntry): Enviroment  = {
    Enviroment(entries ++ List(entry))
  }

  def copyWith(entry: EnvEntry): Enviroment = {
    Enviroment(replaceEntries(List(entry), entries))
  }

  def copyWith(newEntries: List[EnvEntry]): Enviroment = {
    Enviroment(replaceEntries(newEntries, entries))
  }

  def lookup(entry: String, entries: List[EnvEntry]): Option[ValEntry] = {
    entries match {
      case head :: next if head.isInstanceOf[ValEntry] && head.asInstanceOf[ValEntry].identifier == entry => Some(head.asInstanceOf[ValEntry])
      case head :: next                             => lookup(entry, next)
      case Nil                                      => None
    }
  }

  def lookup(entry: String): Option[ValEntry] = {
    lookup(entry, entries)
  }

  def replace(entry: String, _type: Types.Type): Enviroment = {
    entry match {
      case id: String => {
        lookup(id) match {
          case Some(value) =>
            Enviroment(entries.filter(v => v != value)).copyWith(ValEntry(id, _type))
          case None => Enviroment(entries)
        }
      }
      case _ => Enviroment(entries)
    }
  }

  def withType(tree: Trees.Tree, _type: Types.Type) = {
    tree match {
      case Trees.Identifier(v, _) => replace(v, _type)
      case _                      => this
    }
  }
}
