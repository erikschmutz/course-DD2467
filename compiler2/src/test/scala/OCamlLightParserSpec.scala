import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.collection.mutable.Stack
import java.nio.file.{FileSystems, Files}

class OCamlLightParserSpec extends AnyFlatSpec with Matchers {

  def getTokenAsString(value: String): String = {
    OCamlLightParser.parseTokens(value).get.mkString("\n")
  }

  def getSyntaxAsString(value: String) = {
    println( OCamlLightParser.parseSyntax(value))
    // OCamlLightParser.parseSyntax(value).get.mkString("\n")
  }

  "The parser" should "should be able to generate tokens" in {
  }

  val tokens = FileSystems.getDefault.getPath("./src/test/scala/examples/tokens")
  Files
    .list(tokens).sorted()
    .forEach((file) => {
      val fileName = file.toAbsolutePath.toString
      if (fileName.endsWith(".in")) {
        it should "be able to parse tokens from file " + file.getFileName in {
          val inFile = scala.io.Source.fromFile(fileName).mkString;
          val outFile =
            scala.io.Source.fromFile(fileName.replace(".in", ".out")).mkString;
          getTokenAsString(inFile) shouldEqual (outFile)
        } 
      }
    });

  "AST" should "should be able to generate asts" in {
  }



  val syntax = FileSystems.getDefault.getPath("./src/test/scala/examples/syntax")
  Files
    .list(syntax).sorted()
    .forEach((file) => {
      val fileName = file.toAbsolutePath.toString
      if (fileName.endsWith(".in")) {
        it should "be able to parse syntax from file " + file.getFileName in {
          val inFile = scala.io.Source.fromFile(fileName).mkString;
          val outFile =
            scala.io.Source.fromFile(fileName.replace(".in", ".out")).mkString;
          getSyntaxAsString(inFile) shouldEqual (outFile)
          
        } 
      }
    });
}
