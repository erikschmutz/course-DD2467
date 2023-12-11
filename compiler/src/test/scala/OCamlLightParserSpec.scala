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

  "Lexer" should "exists" in {}

  getFilesFromDir("examples/tokens").forEach((file) => {
    val fileName = file.toAbsolutePath.toString
    if (fileName.endsWith(".in")) {
      it should "be able to parse tokens from file Tokens." + file.getFileName in {
        val inFile = readFile(fileName);
        val outFile = readFile(fileName.replace(".in", ".out"));
        getTokens(inFile) shouldEqual (outFile)
      }
    }
  })

  "Parser" should "exists" in {}
  getFilesFromDir("examples/syntax").forEach((file) => {
    val fileName = file.toAbsolutePath.toString
    if (fileName.endsWith(".in")) {
      it should "be able to parse syntax from file " + file.getFileName in {
        val inFile = readFile(fileName);
        // removes white space
        val outFile = readFile(fileName.replace(".in", ".out"))
          .replace(" ", "")
          .replace("\n", "");

        val output = getSyntax(inFile)
          .replace(" ", "")
          .replace("\n", "");

        print(output)
        output shouldEqual (outFile)
      }
    }
  })

}