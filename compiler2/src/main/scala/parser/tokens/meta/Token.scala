import scala.util.matching.Regex
import scala.util.parsing.input.Positional

object TokenMap {
  def set() = {}
  def get() = {}
}


trait Token extends Positional

abstract class Matcher[-A, +B <: Token](regex: Regex, builder: Function[A, B]) {
  def t = builder;
  def r = regex;
  def build(args: A) = builder(args);
}

abstract class StaticMatcher[T <: Token](word: String, t: T)
    extends Matcher[Any, T](word.r, (_) => t) {}

object TokenHelpers {
  def trim(txt: String) = {
    txt.slice(1, txt.length - 1)
  }
}
