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

  def addType(entry: TypeEntry): Enviroment = {
    Enviroment(entries ++ List(entry))
  }

  def addTypes(entry: List[TypeEntry]): Enviroment = {
    Enviroment(entries ++ entry)
  }

  def copyWith(entry: EnvEntry): Enviroment = {
    Enviroment(replaceEntries(List(entry), entries))
  }

  def copyWith(newEntries: List[EnvEntry]): Enviroment = {
    Enviroment(replaceEntries(newEntries, entries))
  }

  def lookup(entry: String, entries: List[EnvEntry]): Option[ValEntry] = {
    entries match {
      case head :: next if head.isInstanceOf[ValEntry] && head.asInstanceOf[ValEntry].identifier == entry =>
        Some(head.asInstanceOf[ValEntry])
      case head :: next => lookup(entry, next)
      case Nil          => None
    }
  }

  def lookup(entry: String): Option[ValEntry] = {
    lookup(entry, entries)
  }

  def types(): List[TypeEntry] = {
    entries.filter(entry => entry.isInstanceOf[TypeEntry]).asInstanceOf[List[TypeEntry]]
  }

  def findType(id: String): Option[TypeEntry] = {
    val types = entries.filter(v => v.isInstanceOf[TypeEntry]).asInstanceOf[List[TypeEntry]]
    types.reverse.foreach(_type => {
      _type._type match {
        case constr: Types.Constraints => {
          constr.constraints.find(p => p.name == id) match {
            case Some(v) => {
              return Some(TypeEntry(v.name, v))
            }
            case None => None
          }
        }
        case _ => None
      }
    })

    types.foreach(_type => {
      if (_type.identifier == id) return Some(_type)
    })
    None
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

  def replaceType(entry: String, _type: Types.Type): Enviroment = {
    Enviroment(
      entries.filter(v => {
        v.isInstanceOf[TypeEntry] && v.asInstanceOf[TypeEntry].identifier != entry
      }) ++ List(TypeEntry(entry, _type))
    )
  }

  def withType(tree: Trees.Tree, _type: Types.Type) = {
    tree match {
      case Trees.Identifier(v, _) => replace(v, _type)
      case _                      => this
    }
  }
  def withType(identifier: String, _type: Types.Type) = {
  
    replaceType(identifier, _type)
  }
}
