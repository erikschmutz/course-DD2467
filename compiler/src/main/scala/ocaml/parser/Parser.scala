/*

  Program           ::= Statement*

  Statement         ::= Expression
                    |  BindingAssignment
                    |  Assignment

  Assignment        ::= Let Identifier Equal Expression
  BindingAssignment ::= Let Identifier+ Equal Expression
  Expression        ::= OperatorExpr
  OperatorExpr      ::= (ParentethisedExpr | Primitive) (Operator (ParentethisedExpr | Primitive))*
  ParentethisedExpr ::= OpenParantheses OperatorExpr CloseParantheses
  Operator          ::= Plus | Minus | Multiply | Divide
  Primitive         ::= IntLit | FloatLit | StringLit | Identifier
  TypedIdentifier   ::= Identifier Colon Identifier
  Identifier        ::= _Identifier | TypedIdentifier




 */
import scala.util.parsing.combinator.{RegexParsers, Parsers}
import ocaml.tokens._
import ocaml.trees._
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}
import scala.util.parsing.input.Positional
import scala.util.parsing.combinator.{PackratParsers, Parsers}

object Parser extends Parsers {

  override type Elem = Tokens.TokenKind

  class TokenReader(tokens: Seq[Tokens.TokenKind]) extends Reader[Tokens.TokenKind] {
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

  object Terminals {
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
      accept(
        "Int",
        { case Tokens.INT_LIT(value) => Trees.Tokens.IntLit(value) }
      )

    val StringLit =
      accept(
        "Int",
        { case Tokens.STR_LIT(value) => Trees.Tokens.StringLit(value) }
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
    val Identifier = accept(
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
    val Multiply = accept(
      "Multiply",
      { case Tokens.MULTIPLY() => Trees.Tokens.Multiply() }
    )
  }

  def TypedIdentifier =
    (
      Terminals.Identifier ~ Terminals.Colon ~ Terminals.Identifier
    ) ^^ { case name ~ _ ~ _type =>
      Trees.Identifier(name.value, Option(_type.value))
    }

  def OperatorExpr: Parser[Trees.Expr] = Term ~ rep(AddExpr | MinusExpr) ^^ { case a ~ b =>
    (a /: b)((acc, f) => f(acc))
  }

  def AddExpr = (Terminals.Plus ~> Term) ^^ { case a =>
    Trees.OperatorExpr(_, a, Trees.Tokens.Plus())
  }

  def MinusExpr = (Terminals.Plus ~> Term) ^^ { case a =>
    Trees.OperatorExpr(_, a, Trees.Tokens.Minus())
  }

  def Term = Factor ~ rep(MultiplyExpr | DivideExpr) ^^ { case a ~ b =>
    (a /: b)((acc, f) => f(acc))
  }

  def MultiplyExpr = (Terminals.Multiply ~> Factor) ^^ { case a =>
    Trees.OperatorExpr(_, a, Trees.Tokens.Multiply())
  }

  def DivideExpr = (Terminals.Divide ~> Factor) ^^ { case a =>
    Trees.OperatorExpr(_, a, Trees.Tokens.Divide())
  }

  def Factor = Primitive | (Terminals.OpenParantheses ~> (OperatorExpr) <~ Terminals.CloseParantheses)

  def Identifier = TypedIdentifier | Terminals.Identifier
  def Operator = Terminals.Plus | Terminals.Minus | Terminals.Multiply | Terminals.Divide
  def Primitive = Terminals.IntLit | Terminals.FloatLit | Terminals.StringLit | Terminals.Identifier;

  def Assignment = Terminals.Let ~ Identifier ~ Terminals.Equal ~ Expression ^^ { case (_ ~ identifier ~ _ ~ value) =>
    Trees.Assignment(identifier, value)
  }
// f 10

  def Substitution: Parser[Trees.Tree => Trees.Substitutions] = Expression ^^ {
    case value => {
      Trees.Substitutions(_, value)
    }
  }

  def Substitutions: Parser[Trees.Expr] = Identifier ~ rep1(Substitution) ^^ {
    case id ~ substitutions => {
      (id.asInstanceOf[Trees.Expr] /: substitutions)((acc, f) => f(acc))
    }
  }

  def BindingAssignment =
    Terminals.Let ~ repNM(
      2,
      Integer.MAX_VALUE,
      Identifier
    ) ~ Terminals.Equal ~ Expression ^^ { case (_ ~ identifier ~ _ ~ value) =>
      Trees.Assignment(
        identifier.head,
        Trees.LetBinding(identifier.tail, value)
      )
    }

  def Expression = Substitutions | OperatorExpr
  def Definition = (Assignment)
  def Program = (Expression | Definition).* ^^ { case list =>
    Trees.Program(list)
  };

  def evaluate(tokens: List[Tokens.TokenKind]) = {
    phrase(Program)(new TokenReader(tokens))
  }

}
