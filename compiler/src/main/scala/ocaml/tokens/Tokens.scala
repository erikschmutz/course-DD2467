package ocaml.tokens

import scala.util.matching.Regex
import scala.collection.immutable.HashMap
import scala.util.parsing.input.Positional

object Tokens {
  abstract class TokenKind extends Positional
  abstract class MetaToken extends TokenKind
  abstract class Token[T](value: T) extends TokenKind

  case class COMMENT(value: String) extends MetaToken

  /* Literals */
  case class INT_LIT(value: Integer) extends Token(value)
  case class FLOAT_LIT(value: Float) extends Token(value)
  case class CHAR_LIT(value: String) extends Token(value)
  case class STR_LIT(value: String) extends Token(value)

  /* Identifiers */
  case class IDENTIFIER(value: String) extends Token(value)

  /* Keywords */
  case class LET() extends TokenKind
  case class REC() extends TokenKind
  case class IN() extends TokenKind
  case class IF() extends TokenKind
  case class THEN() extends TokenKind
  case class ELSE() extends TokenKind
  case class FUNCTION() extends TokenKind
  case class ARROW() extends TokenKind
  case class GT() extends TokenKind
  case class LT() extends TokenKind
  case class TRUE() extends TokenKind
  case class FALSE() extends TokenKind
  case class UNIT() extends TokenKind

  /* Operators */
  case class EQUAL() extends TokenKind
  case class DIVIDER() extends TokenKind
  case class DIVIDE() extends TokenKind
  case class PLUS() extends TokenKind
  case class MINUS() extends TokenKind
  case class MULTIPLY() extends TokenKind

  case class FLOAT_PLUS() extends TokenKind
  case class FLOAT_MINUS() extends TokenKind
  case class FLOAT_DIVIDE() extends TokenKind
  case class FLOAT_MULTIPLY() extends TokenKind

  case class OPEN_PARENTETHES() extends TokenKind
  case class CLOSE_PARENTETHES() extends TokenKind
  case class COLON() extends TokenKind
  case class SEMICOLON() extends TokenKind
}
