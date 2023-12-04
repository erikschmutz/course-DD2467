/*

Syntax:
  primitive                 := CHAR | STRING | INT | FLOAT
  parentethiesed_expr       := "(" + operator_expr + ")"
  operator_expr             := chainl1((primitive | parentethiesed_expr), binary_operator)
  expr                      :=  primitive                         |
                              operator_expr                       |
                              parentethiesed_expr                 |
  binary_operator           := * | / | + | -
  assignement               := let identifier = expr
 */
import scala.util.parsing.combinator.{RegexParsers, Parsers}
import ocaml.tokens._
import ocaml.trees._
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}
import scala.util.parsing.input.Positional
import scala.util.parsing.combinator.{PackratParsers, Parsers}

object Parser extends PackratParsers {

  override type Elem = Tokens.TokenKind

  class TokenReader(tokens: Seq[Tokens.TokenKind])
      extends Reader[Tokens.TokenKind] {
    override def first: Tokens.TokenKind = tokens.head
    override def atEnd: Boolean = {

      tokens.isEmpty
    }
    override def pos: Position =
      tokens.headOption.map(_.pos).getOrElse(NoPosition)
    override def rest: Reader[Tokens.TokenKind] = {
      println("tokens", tokens.tail)
      new TokenReader(tokens.tail)
    }
  }

  def OpenParantheses =
    accept(
      "Parentensis",
      { case Tokens.OPEN_PARENTETHES() => Trees.Tokens.OpenParantheses() }
    )
  def CloseParantheses =
    accept(
      "Close Parantheses",
      { case Tokens.CLOSE_PARENTETHES() => Trees.Tokens.CloseParantheses() }
    )
  def IntLit =
    log(
      accept(
        "Int",
        { case Tokens.INT_LIT(value) => Trees.Tokens.IntLit(value) }
      )
    )(
      "IntLit"
    )
  def StringLit =
    log(
      accept(
        "Int",
        { case Tokens.STR_LIT(value) => Trees.Tokens.StringLit(value) }
      )
    )(
      "IntLit"
    )
  def FloatLit =
    accept(
      "FloatLit",
      { case Tokens.FLOAT_LIT(value) => Trees.Tokens.FloatLit(value) }
    )
  def Plus = accept(
    "Plus",
    { case Tokens.PLUS() => Trees.Tokens.Plus() }
  )
  def Divide = accept(
    "Plus",
    { case Tokens.DIVIDE() => Trees.Tokens.Divide() }
  )
  def Minus = accept(
    "Minus",
    { case Tokens.MINUS() => Trees.Tokens.Minus() }
  )
  def Let = accept(
    "Let",
    { case Tokens.LET() => Trees.Tokens.Let() }
  )

  def Equal = accept(
    "Equal",
    { case Tokens.EQUAL() => Trees.Tokens.Equal() }
  )

  def Identifier = accept(
    "Identifier",
    { case Tokens.IDENTIFIER(value) => Trees.Identifier(value) }
  )

  def Multiply = accept(
    "Multiply",
    { case Tokens.MULTIPLY() => Trees.Tokens.Multiply() }
  )

  def Operator = Plus | Minus | Multiply | Divide

  def Primitive = IntLit | FloatLit | StringLit | Identifier;

  def ParentethisedExpr =
    OpenParantheses ~> (OperatorExpr) <~ CloseParantheses ^^ { case a =>
      Trees.Parentensis(a)
    }

  def Assignment = Let ~ Identifier ~ Equal ~ Expression ^^ {
    case (_ ~ identifier ~ _ ~ value) =>
      Trees.Assignment(identifier, value)
  }

  lazy val OperatorExpr: PackratParser[Trees.Expr] =
    chainl1(
      (ParentethisedExpr | Primitive),
      Operator ^^ { op => (l, r) =>
        Trees.OperatorExpr(l, r, op)
      }
    )

  lazy val Expression: PackratParser[Trees.Expr] =
    OperatorExpr

  lazy val Statement: PackratParser[Trees.Expr] =
    Expression | Assignment

  lazy val Program: PackratParser[Trees.Program] = Statement.* ^^ { case list =>
    Trees.Program(list)
  };

  def evaluate(tokens: List[Tokens.TokenKind]) = {
    println(">>>>>>>>>", tokens, "<<<<<<<<<<")
    val result = phrase(Program)(new PackratReader(new TokenReader(tokens)))
    println(">>", result)
    result
  }

}
