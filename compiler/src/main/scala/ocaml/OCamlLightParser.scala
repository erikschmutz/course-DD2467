object OCamlLightParser {
  def parseTokens(value: String) = {
    Lexer.evaluate(value)
  }

  def parseSyntax(value: String) = {
    val tokens = parseTokens(value).get;
    Parser.evaluate(tokens)
  }

  def analyze(value: String) = {
    val tree = parseSyntax(value).get
    Analyzer.evaluate(tree)
  }
}
