import scala.util.parsing.combinator.{RegexParsers, Parsers}
import scala.util.parsing.input.Positional
import scala.reflect.{ClassTag, classTag}
import scala.collection.mutable.HashMap
import scala.util.parsing.combinator.{PackratParsers, Parsers}

case class IntTree(value: Integer) extends Tree

object Utils {
  var hashMap = new HashMap[Any, Any]()
}

abstract class SyntaxDefinition extends Parsers with PackratParsers{
  override type Elem = Token
  type Output

  if (!Utils.hashMap.contains(this.getClass())) {
    println("setup", this.getClass().getName())
    Utils.hashMap.addOne((this.getClass().getName(), this))
  }

  def definition: PackratParser[Output];

  def t[T <: SyntaxDefinition](value: () => T) = {
    val name = value.getClass().getName().replace("$", "")
     
    if (!Utils.hashMap.contains(name)) {
      val v = value()
        val result = v.definition.asInstanceOf[this.PackratParser[v.Output]]
        result
    }else{
      val v = Utils.hashMap.get(name).get.asInstanceOf[T];
      val result = v.definition.asInstanceOf[this.PackratParser[v.Output]]
      
      if (Utils.hashMap.contains(name + ".t")){
        Utils.hashMap.get(name + ".t").get.asInstanceOf[this.PackratParser[v.Output]]
      }else{ 
        Utils.hashMap.addOne((name + ".t", result))
        println("Hashmap", Utils.hashMap)
        result
      }
    }
  }

  def t[T <: Token: ClassTag](v: (_) => T) = {
    positioned {
      accept(
        "literal",
        {
          case lit @ value if classTag[T].runtimeClass.isInstance(lit) => lit
        }
      )
    }
  }
}
