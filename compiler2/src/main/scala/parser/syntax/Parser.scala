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
      new TokenReader(tokens.tail)
    }
  }

  val OpenParantheses =
    accept(
      "Parentensis",
      { case Tokens.OPEN_PARENTETHES() => Trees.Tokens.OpenParantheses() }
    )
  val CloseParantheses =
    accept(
      "Close Parantheses",
      { case Tokens.CLOSE_PARENTETHES() => Trees.Tokens.CloseParantheses() }
    )
  val IntLit =
    log(
      accept(
        "Int",
        { case Tokens.INT_LIT(value) => Trees.Tokens.IntLit(value) }
      )
    )(
      "IntLit"
    )
  val StringLit =
    log(
      accept(
        "Int",
        { case Tokens.STR_LIT(value) => Trees.Tokens.StringLit(value) }
      )
    )(
      "IntLit"
    )
  val FloatLit =
    accept(
      "FloatLit",
      { case Tokens.FLOAT_LIT(value) => Trees.Tokens.FloatLit(value) }
    )
  val Plus = accept(
    "Plus",
    { case Tokens.PLUS() => Trees.Tokens.Plus() }
  )
  val Divide = accept(
    "Plus",
    { case Tokens.DIVIDE() => Trees.Tokens.Divide() }
  )
  val Minus = accept(
    "Minus",
    { case Tokens.MINUS() => Trees.Tokens.Minus() }
  )
  val Let = accept(
    "Let",
    { case Tokens.LET() => Trees.Tokens.Let() }
  )
  val Equal = accept(
    "Equal",
    { case Tokens.EQUAL() => Trees.Tokens.Equal() }
  )
  val _Identifier = accept(
    "Identifier",
    { case Tokens.IDENTIFIER(value) =>
      Trees.Identifier(value, Option.empty)
    }
  )
  val Colon = accept(
    "Identifier",
    { case Tokens.COLON() =>
      Trees.Tokens.Colon()
    }
  )
  val TypedIdentifier = (_Identifier ~ Colon ~ _Identifier) ^^ {
    case name ~ _ ~ _type =>
      Trees.Identifier(name.value, Option(_type.value))
  }
  val Identifier = TypedIdentifier | _Identifier

  val Multiply = accept(
    "Multiply",
    { case Tokens.MULTIPLY() => Trees.Tokens.Multiply() }
  )
  val Operator = Plus | Minus | Multiply | Divide
  val Primitive = IntLit | FloatLit | StringLit | Identifier;
  val ParentethisedExpr =
    OpenParantheses ~> (OperatorExpr) <~ CloseParantheses ^^ { case a =>
      Trees.Parentensis(a)
    }
  val Assignment = Let ~ Identifier ~ Equal ~ Expression ^^ {
    case (_ ~ identifier ~ _ ~ value) =>
      Trees.Assignment(identifier, value)
  }
  val BindingAssignment =
    Let ~ repN(2, Identifier) ~ Equal ~ Expression ^^ {
      case (_ ~ identifier ~ _ ~ value) =>
        Trees.Assignment(
          identifier.head,
          Trees.LetBinding(identifier.tail, value)
        )
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
    Expression |
      BindingAssignment |
      Assignment

  lazy val Program: PackratParser[Trees.Program] = Statement.* ^^ { case list =>
    Trees.Program(list)
  };

  def evaluate(tokens: List[Tokens.TokenKind]) = {
    print(tokens)
    phrase(Program)(new PackratReader(new TokenReader(tokens)))

  }

}
