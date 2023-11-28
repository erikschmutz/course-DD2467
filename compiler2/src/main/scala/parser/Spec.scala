import scala.util.matching.Regex

object Spec {
  def TOKENS = (
    (IntLiteral, IntLiteralMatcher()),
    (Let, LetMatcher()),
    (IdentifierLowecaseIndent, IdentifierLowecaseIndentMatcher()),
    (Equal, EqualMatcher()),
    (CharLiteral, CharLiteralMatcher()),
    (Plus, PlusMatcher()),
    (StringLiteral, StringLiteralMatcher())
  );

  def t(token: (_) => Token) = {
    val index = Array
      .from(TOKENS.productIterator)
      .map((v) => v.asInstanceOf[Tuple2[(_) => Token, Matcher[Any, Token]]]._1)
      .indexOf(token);

    ParserUtils.getParser(matchers(index))
  }

  def syntax = Array
    .from(TOKENS.productIterator)
    .map((v) => v.asInstanceOf[Tuple2[() => Token, Matcher[Any, Token]]]._2);

  def tokens = Array
    .from(TOKENS.productIterator)
    .map((v) => v.asInstanceOf[Tuple2[() => Token, Matcher[Any, Token]]]._1);

  def matchers = Array
    .from(TOKENS.productIterator)
    .map((v) => v.asInstanceOf[Tuple2[() => Token, Matcher[Any, Token]]]._2);

}
