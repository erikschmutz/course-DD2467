package ocaml.enviroment

import ocaml.types._
import ocaml.trees.Trees

case class EnviromentEntry(identifier: String, _type: Types.Type) extends PrettyPrint {
  def prettyPrint: String = "val " + identifier + " : " + _type.prettyPrint

  override def toString = {
    "EnviromentEntry(Identifier(" + identifier + ")," + _type + ")"
  }

}

case class Enviroment(entries: List[EnviromentEntry]) extends PrettyPrint {
  def prettyPrint: String = {
    return entries.map(entry => entry.prettyPrint).mkString("\n")
  }

  def replaceEntries(newEntries: List[EnviromentEntry], entries: List[EnviromentEntry]): List[EnviromentEntry] = {
    val filtered = entries.filter(entry => !newEntries.contains(entry));
    filtered ::: newEntries
  }

  def copyWith(entry: EnviromentEntry): Enviroment = {
    Enviroment(replaceEntries(List(entry), entries))
  }

  def copyWith(newEntries: List[EnviromentEntry]): Enviroment = {
    Enviroment(replaceEntries(newEntries, entries))
  }

  def lookup(entry: String, entries: List[EnviromentEntry]): Option[EnviromentEntry] = {
    entries match {
      case head :: next if head.identifier == entry => Some(head)
      case head :: next                             => lookup(entry, next)
      case Nil                                      => None
    }
  }

  def lookup(entry: String): Option[EnviromentEntry] = {
    lookup(entry, entries)
  }

  def replace(entry: String, _type: Types.Type): Enviroment = {
    entry match {
      case id: String => {
        lookup(id) match {
          case Some(value) =>
            Enviroment(entries.filter(v => v != value)).copyWith(EnviromentEntry(id, _type))
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
