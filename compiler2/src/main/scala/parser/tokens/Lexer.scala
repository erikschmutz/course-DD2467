import scala.util.parsing.combinator.{RegexParsers, Parsers}
import ocaml.tokens._

object Lexer extends RegexParsers {

  def strip(text: String) = {
    text.slice(1, text.length - 1)
  }

  // Key words
  def LET = "let".r ^^ { _ => Tokens.LET() }

  // operators
  def EQUAL = "=".r ^^ { _ => Tokens.EQUAL() }
  def PLUS = "\\+".r ^^ { _ => Tokens.PLUS() }
  def MINUS = "\\-".r ^^ { _ => Tokens.MINUS() }
  def MULTIPLY = "\\*".r ^^ { _ => Tokens.MULTIPLY() }
  def DIVIDE = "\\/".r ^^ { _ => Tokens.DIVIDE() }

  def OPEN_PARENTETHES = "\\(".r ^^ { _ => Tokens.OPEN_PARENTETHES() }
  def CLOSE_PARENTETHES = "\\)".r ^^ { _ => Tokens.CLOSE_PARENTETHES() }

  // literals and identifiers
  def INT_LIT = """\d+(\.\d*)?""".r ^^ { a => Tokens.INT_LIT(a.toInt) }
  def FLOAT_LIT = """([0-9])*\.([0-9])*""".r ^^ { a =>
    Tokens.FLOAT_LIT(a.toFloat)
  }
  def STR_LIT = "\".*\"".r ^^ { a => Tokens.STR_LIT(a.slice(1, a.length - 1)) }
  def CHAR_LIT = "\'.*\'".r ^^ { a => Tokens.CHAR_LIT(strip(a)) }
  def IDENTIFIER = "[a-z]".r ^^ { a => Tokens.IDENTIFIER(a) }

  def ALL = positioned {
    (
      OPEN_PARENTETHES |
        CLOSE_PARENTETHES |
        LET |
        EQUAL |
        PLUS |
        MINUS |
        MULTIPLY |
        FLOAT_LIT |
        INT_LIT |
        STR_LIT |
        CHAR_LIT |
        DIVIDE |
        IDENTIFIER
    )
  }

  def evaluate(text: String) = {
    parseAll(rep(ALL), text)
  }
}
