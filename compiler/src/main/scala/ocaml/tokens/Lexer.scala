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
  def OF = "of ".r ^^ { _ => Tokens.OF() }
  def TYPE = "type ".r ^^ { _ => Tokens.TYPE() }
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
  def SEMICOLON = ";".r ^^ { _ => Tokens.SEMICOLON() }
  def LINE = "\\|".r ^^ { _ => Tokens.LINE() }
  def PLUS = "\\+".r ^^ { _ => Tokens.PLUS() }
  def FLOAT_PLUS = "\\+\\.".r ^^ { _ => Tokens.FLOAT_PLUS() }
  def MINUS = "\\-".r ^^ { _ => Tokens.MINUS() }
  def MULTIPLY = "\\*".r ^^ { _ => Tokens.MULTIPLY() }
  def DIVIDE = "\\/".r ^^ { _ => Tokens.DIVIDE() }

  def ARRAY_OPEN = "\\[\\|".r ^^ { _ => Tokens.ARRAY_OPEN() }
  def ARRAY_CLOSE = "\\|\\]".r ^^ { _ => Tokens.ARRAY_CLOSE() }
  def OPEN_PARENTETHES = "\\(".r ^^ { _ => Tokens.OPEN_PARENTETHES() }
  def CLOSE_PARENTETHES = "\\)".r ^^ { _ => Tokens.CLOSE_PARENTETHES() }
  def OPEN_CURLY = "\\{".r ^^ { _ => Tokens.OPEN_CURLY() }
  def CLOSE_CURLY = "\\}".r ^^ { _ => Tokens.CLOSE_CURLY() }
  def COLON = ":".r ^^ { _ => Tokens.COLON() }
  def UNIT = "\\(\\)".r ^^ { _ => Tokens.UNIT() }
  def COMMA = ",".r ^^ { _ => Tokens.COMMA() }

  // literals and identifiers
  def INT_LIT = """\d+(\.\d*)?""".r ^^ { a => Tokens.INT_LIT(a.toInt) }
  def FLOAT_LIT = """([0-9])*\.([0-9])*""".r ^^ { a =>
    Tokens.FLOAT_LIT(a.toFloat)
  }
  def STR_LIT = "\".*\"".r ^^ { a => Tokens.STR_LIT(a.slice(1, a.length - 1)) }
  def CHAR_LIT = "\'.\'".r ^^ { a => Tokens.CHAR_LIT(strip(a)) }
  def IDENTIFIER = "[a-z]+".r ^^ { a => Tokens.IDENTIFIER(a) }
  def CONSTR_IDENTIFIER = "[A-Z][a-zA-Z]+".r ^^ { a => Tokens.CONSTR_IDENTIFIER(a) }
  def TYPE_PARAM = "\'[a-z]+".r ^^ { a => Tokens.TYPE_PARAM(a) }

  def ALL = positioned {
          ARRAY_OPEN |
      ARRAY_CLOSE |
    UNIT |
      COMMENT |
      REC |
      TRUE |
      OF |
      FALSE |
      OPEN_PARENTETHES |
      OPEN_CURLY |
      CLOSE_CURLY |
      DIVIDER |
      SEMICOLON |
      COMMA |
      CLOSE_PARENTETHES |
      LET |
      LT |
      TYPE |
      GT |
      IF |
      THEN |
      ELSE |
      IN |
      ARROW |
      FUNCTION |
      LINE |
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
      TYPE_PARAM | 
      DIVIDE |
      IDENTIFIER | 
      CONSTR_IDENTIFIER
  }

  def evaluate(text: String) = {
    parseAll(rep(ALL), text) match {
      case Success(result, next) => Some(result.filter(!_.isInstanceOf[Tokens.COMMENT]))
      case _                     => None
    }
  }
}
