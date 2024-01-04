import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.collection.mutable.Stack
import java.nio.file.{FileSystems, Files}

class OCamlLightParserSpec extends AnyFlatSpec with Matchers {

  def readFile(path: String): String = {
    scala.io.Source.fromFile(path).mkString
  }

  def getFilesFromDir(dir: String) = {
    val path =
      FileSystems.getDefault.getPath("./src/test/scala/" + dir)

    Files
      .list(path)
      .sorted()
  }

  def getTokens(value: String): String = {
    OCamlLightParser.parseTokens(value).get.mkString("\n")
  }

  def getSyntax(value: String): String = {
    OCamlLightParser
      .parseSyntax(value)
      .get
      .toString
  }

  def getAnalyzer(value: String): String = {
    OCamlLightParser
      .analyze(value) match {
      case None        => None.toString()
      case Some(value) => value.toString()
    }
  }

  def getAnalyzerPretty(value: String): String = {
    OCamlLightParser
      .analyze(value) match {
      case None        => None.toString()
      case Some(value) => value.prettyPrint
    }
  }

  "Lexer" should "exists" in {}

  getFilesFromDir("examples/valid").forEach((file) => {
    val fileName = file.toAbsolutePath.toString
    if (fileName.endsWith(".in")) {
      it should "be able to parse tokens from file Tokens." + file.getFileName in {
        val inFile = readFile(fileName);
        val outFile = readFile(fileName.replace(".in", ".tokens"))
          .replace(" ", "")
          .replace("\n", "");;
        getTokens(inFile)
          .replace(" ", "")
          .replace("\n", "") shouldEqual (outFile)
      }
    }
  })

  "Parser" should "exists" in {}
  getFilesFromDir("examples/valid").forEach((file) => {
    val fileName = file.toAbsolutePath.toString
    if (fileName.endsWith(".in")) {
      it should "be able to parse syntax from file Syntax." + file.getFileName in {
        val inFile = readFile(fileName);
        // removes white space
        val outFile = readFile(fileName.replace(".in", ".syntax"))
          .replace(" ", "")
          .replace("\n", "");

        val output = getSyntax(inFile)
          .replace(" ", "")
          .replace("\n", "");

        output shouldEqual (outFile)
      }
    }
  })

  // "Analyzer" should "exists" in {}

  getFilesFromDir("examples/valid").forEach((file) => {
    val fileName = file.toAbsolutePath.toString
    if (fileName.endsWith(".in")) {
      it should "be able to analyze syntax from file Types." + file.getFileName in {
        val inFile = readFile(fileName);
        // removes white space
        val outFile = readFile(fileName.replace(".in", ".types"))
          .replace(" ", "")
          .replace("\n", "");

        val output = getAnalyzer(inFile)
          .replace(" ", "")
          .replace("\n", "");
        println("output", output)
        // print(getAnalyzerPretty(inFile))
        output shouldEqual (outFile)
      }
    }
  })

}
