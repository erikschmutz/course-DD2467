case class IntLiteralMatcher()
    extends Matcher[String, IntLiteral](
      """\d+(\.\d*)?""".r,
      (a) => IntLiteral(Integer.parseInt(a))
    );

case class StringLiteralMatcher()
    extends Matcher[String, StringLiteral](
      "\".*\"".r,
      (a) => StringLiteral(TokenHelpers.trim(a))
    );

case class CharLiteralMatcher()
    extends Matcher[String, CharLiteral](
      "\'.*\'".r,
      (a) => CharLiteral(TokenHelpers.trim(a))
    );

case class IdentifierLowecaseIndentMatcher()
    extends Matcher[String, IdentifierLowecaseIndent](
      "[a-z]".r,
      (a) => IdentifierLowecaseIndent(a)
    );

case class LetMatcher() extends StaticMatcher("let", Let());

case class EqualMatcher() extends StaticMatcher("=", Equal());
case class PlusMatcher() extends StaticMatcher("\\+", Plus());
