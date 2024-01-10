import scala.util.parsing.combinator.{RegexParsers, Parsers}
import ocaml.tokens._

object Lexer extends RegexParsers {

  def strip(text: String) = {
    text.slice(1, text.length - 1)
  }

  // Key words
  def LET = "let ".r ^^ { _ => Tokens.LET() }
  def IF = "if ".r ^^ { _ => Tokens.IF() }
  def THEN = "then ".r ^^ { _ => Tokens.THEN() }
  def REC = "rec ".r ^^ { _ => Tokens.REC() }
  def TRUE = "true ".r ^^ { _ => Tokens.TRUE() }
  def FALSE = "false ".r ^^ { _ => Tokens.FALSE() }
  def ELSE = "else ".r ^^ { _ => Tokens.ELSE() }
  def LT = "<".r ^^ { _ => Tokens.LT() }
  def GT = ">".r ^^ { _ => Tokens.GT() }
  def IN = "in ".r ^^ { _ => Tokens.IN() }
  def FUNCTION = "function".r ^^ { _ => Tokens.FUNCTION() }
  def ARROW = "->".r ^^ { _ => Tokens.ARROW() }
  def COMMENT = "\\(\\*(.|\\n)*\\*\\)".r ^^ { a => Tokens.COMMENT(a) }

  // operators
  def EQUAL = "=".r ^^ { _ => Tokens.EQUAL() }
  def DIVIDER = ";;".r ^^ { _ => Tokens.DIVIDER() }
  def PLUS = "\\+".r ^^ { _ => Tokens.PLUS() }
  def FLOAT_PLUS = "\\+\\.".r ^^ { _ => Tokens.FLOAT_PLUS() }
  def MINUS = "\\-".r ^^ { _ => Tokens.MINUS() }
  def MULTIPLY = "\\*".r ^^ { _ => Tokens.MULTIPLY() }
  def DIVIDE = "\\/".r ^^ { _ => Tokens.DIVIDE() }

  def OPEN_PARENTETHES = "\\(".r ^^ { _ => Tokens.OPEN_PARENTETHES() }
  def CLOSE_PARENTETHES = "\\)".r ^^ { _ => Tokens.CLOSE_PARENTETHES() }
  def COLON = ":".r ^^ { _ => Tokens.COLON() }
  def UNIT = "\\(\\)".r ^^ { _ => Tokens.UNIT() }

  // literals and identifiers
  def INT_LIT = """\d+(\.\d*)?""".r ^^ { a => Tokens.INT_LIT(a.toInt) }
  def FLOAT_LIT = """([0-9])*\.([0-9])*""".r ^^ { a =>
    Tokens.FLOAT_LIT(a.toFloat)
  }
  def STR_LIT = "\".*\"".r ^^ { a => Tokens.STR_LIT(a.slice(1, a.length - 1)) }
  def CHAR_LIT = "\'.*\'".r ^^ { a => Tokens.CHAR_LIT(strip(a)) }
  def IDENTIFIER = "[a-z]+".r ^^ { a => Tokens.IDENTIFIER(a) }

  def ALL = positioned {
    UNIT |
      COMMENT |
      REC |
      TRUE |
      FALSE |
      OPEN_PARENTETHES |
      DIVIDER |
      CLOSE_PARENTETHES |
      LET |
      LT |
      GT |
      IF |
      THEN |
      ELSE |
      IN |
      ARROW |
      FUNCTION |
      EQUAL |
      FLOAT_PLUS |
      PLUS |
      MINUS |
      COLON |
      MULTIPLY |
      FLOAT_LIT |
      INT_LIT |
      STR_LIT |
      CHAR_LIT |
      DIVIDE |
      IDENTIFIER
  }

  def evaluate(text: String) = {
    parseAll(rep(ALL), text) match {
      case Success(result, next) => Some(result.filter(!_.isInstanceOf[Tokens.COMMENT]))
      case _                     => None
    }
  }
}
