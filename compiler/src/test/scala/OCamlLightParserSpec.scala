import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.collection.mutable.Stack
import java.nio.file.{FileSystems, Files}
import java.io.PrintWriter
import java.io.File
import java.io.FileOutputStream

class OCamlLightParserSpec extends AnyFlatSpec with Matchers {

  def readFile(path: String): String = {
    scala.io.Source.fromFile(path).mkString
  }

  def writeToFile(path: String, value: String) = {
    val printWriter = new PrintWriter(new FileOutputStream(new File(path)))
    printWriter.write(value)
    printWriter.close()
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

  def getAnalyzer(value: String) = {
    OCamlLightParser
      .analyze(value) match {
      case None        => None
      case Some(value) => value
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

  getFilesFromDir("examples/tokens").forEach((file) => {
    val fileName = file.toAbsolutePath.toString
    if (fileName.endsWith(".in")) {
      it should "be able to parse tokens from file Tokens." + file.getFileName in {
        val inFile = readFile(fileName);
        val outFile = readFile(fileName.replace(".in", ".out"))
          .replace(" ", "")
          .replace("\n", "");;
        val output = getTokens(inFile)
          .replace(" ", "")
          .replace("\n", "")

        println(output)
        output shouldEqual (outFile)
      }
    }
  })

  "Parser" should "exists" in {}
  getFilesFromDir("examples/syntax").forEach((file) => {
    val fileName = file.toAbsolutePath.toString
    if (fileName.endsWith(".in")) {
      it should "be able to parse syntax from file Syntax." + file.getFileName in {
        val inFile = readFile(fileName);
        // removes white space
        val outFile = readFile(fileName.replace(".in", ".out"))
          .replace(" ", "")
          .replace("\n", "");

        val output = getSyntax(inFile)
          .replace(" ", "")
          .replace("\n", "");
        println("output", output)
        output shouldEqual (outFile)
      }
    }
  })

  // "Analyzer" should "exists" in {}

  getFilesFromDir("examples/analyzer").forEach((file) => {
    val fileName = file.toAbsolutePath.toString
    if (fileName.endsWith(".in")) {
      it should "be able to analyze syntax from file Types." + file.getFileName in {
        val inFile = readFile(fileName);
        // removes white space
        val outFile = readFile(fileName.replace(".in", ".out"))
          .replace(" ", "")
          .replace("\n", "");

        val output = getAnalyzer(inFile)

        val outputStr = output
          .toString()
          .replace(" ", "")
          .replace("\n", "");

        outputStr shouldEqual (outFile)

        println(output)

        // if we succeed we write out the pretty file
        val pretty = getAnalyzerPretty(inFile)
        writeToFile(
          fileName,
          inFile.split("\\(\\*")(0) + "(*====================\n" + pretty + "\n====================*)"
        )
      }
    }
  })

}
