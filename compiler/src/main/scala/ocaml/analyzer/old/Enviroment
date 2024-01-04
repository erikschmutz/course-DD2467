package ocaml.enviroment
import ocaml.trees._

import ocaml.types._

case class EnviromentEntry(identifier: Trees.Identifier, _type: Types.Type, tree: Trees.Tree) extends PrettyPrint {
  def prettyPrint: String = "val " + identifier.value + " : " + _type.prettyPrint

  override def toString = {
    "EnviromentEntry(" + identifier + "," + _type + ")"
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

  def lookup(entry: Trees.Identifier, entries: List[EnviromentEntry]): Option[EnviromentEntry] = {
    entries match {
      case head :: next if head.identifier == entry => Some(head)
      case head :: next                             => lookup(entry, next)
      case Nil                                      => None
    }
  }

  def lookup(entry: Trees.Identifier): Option[EnviromentEntry] = {
    lookup(entry, entries)
  }

  def replace(entry: Trees.Tree, _type: Types.Type): Enviroment = {
    entry match {
      case id: Trees.Identifier => {
        lookup(id) match {
          case Some(value) =>
            Enviroment(entries.filter(v => v != value)).copyWith(EnviromentEntry(id, _type, entry))
          case None => Enviroment(entries)
        }
      }
      case _ => Enviroment(entries)
    }
  }
}
