package ocaml.enviroment
import ocaml.trees._

import ocaml.types._

case class EnviromentEntry(identifier: Trees.Identifier, _type: Types.Type)

case class Enviroment(entries: List[EnviromentEntry]) {
  def copyWith(entry: EnviromentEntry) = {
    new Enviroment(entries ::: List(entry))
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
}
