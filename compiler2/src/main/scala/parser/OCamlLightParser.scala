object OCamlLightParser {
  def parseTokens(value: String) = {
    Lexer.evaluate(value)
  }

  def parseSyntax(value: String) = {
    val tokens = parseTokens(value).get;
    Parser.evaluate(tokens)
  }
}
