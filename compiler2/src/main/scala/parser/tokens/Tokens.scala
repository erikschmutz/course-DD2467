package ocaml.tokens

import scala.util.matching.Regex
import scala.collection.immutable.HashMap
import scala.util.parsing.input.Positional

object Tokens {
  abstract class TokenKind extends Positional
  abstract class Token[T](value: T) extends TokenKind

  /* Literals */
  case class INT_LIT(value: Integer) extends Token(value)
  case class FLOAT_LIT(value: Float) extends Token(value)
  case class CHAR_LIT(value: String) extends Token(value)
  case class STR_LIT(value: String) extends Token(value)

  /* Identifiers */
  case class IDENTIFIER(value: String) extends Token(value)

  /* Keywords */
  case class LET() extends TokenKind

  /* Operators */
  case class EQUAL() extends TokenKind
  case class DIVIDE() extends TokenKind
  case class PLUS() extends TokenKind
  case class MINUS() extends TokenKind
  case class MULTIPLY() extends TokenKind

  case class OPEN_PARENTETHES() extends TokenKind
  case class CLOSE_PARENTETHES() extends TokenKind
  case class COLON() extends TokenKind
}
