import scala.util.parsing.combinator.{RegexParsers, Parsers}

object OCamlLightParser extends RegexParsers {
  def parseTokens(value: String) = {
    val syntax =
      Spec.matchers
        .map((p) => ParserUtils.getParser(p))
        .reduce(((a, b) => a | b));

    ParserUtils.parseAll(ParserUtils.rep(syntax), value)
  }

  def parseSyntax(value: String) = {
    val tokens = parseTokens(value).get;
    println(tokens)
    val result = AST.apply(tokens)
    print("result", result)
    result
  }
}
