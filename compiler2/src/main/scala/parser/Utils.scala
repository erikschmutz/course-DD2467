import scala.util.parsing.combinator.{RegexParsers, Parsers}

object ParserUtils extends RegexParsers {
  def getParser(token: Matcher[Any, Token]): Parser[Token] = {
    token.r ^^ { token.build(_) }
  }
}


