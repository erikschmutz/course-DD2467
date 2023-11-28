/* Literals */
case class IntLiteral(value: Integer) extends Token {}
case class CharLiteral(value: String) extends Token {}
case class StringLiteral(value: String) extends Token {}

/* Identifiers */
case class IdentifierLowecaseIndent(value: String) extends Token {}
case class IdentifierCapitalizedIndent(value: String) extends Token {}
case class Identifier(value: String) extends Token {}

/* Keywords */
case class Let() extends Token {}

/* Operators */
case class Equal() extends Token() {}
case class Plus() extends Token() {}
case class Minus() extends Token() {}
case class Multiply() extends Token() {}
