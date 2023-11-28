import scala.util.parsing.combinator.{RegexParsers, Parsers}

object Lexer extends RegexParsers {
  abstract class Token

  /* Literals */
  case class INT_LIT(value: Integer) extends Token
  case class CHAR_LIT(value: String) extends Token
  case class STR_LIT(value: String) extends Token

  /* Identifiers */
  case class IDENTIFIER(value: String) extends Token

  /* Keywords */
  case class LET() extends Token

  /* Operators */
  case class EQUAL() extends Token
  case class PLUS() extends Token
  case class MINUS() extends Token
  case class MULTIPLY() extends Token

}
