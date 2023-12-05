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
  _Identifier       ::= _Identifier
  IntLit            ::= IntLit
  FloatLit          ::= FloatLit
  StringLit         ::= StringLit
  Colon             ::= Colon
  Plus              ::= Plus

  Divide            ::= Divide

  Minus             ::= Minus

  Let               ::= Let

  Equal             ::= Equal

  OpenParantheses   ::= OpenParantheses

  CloseParantheses  ::= CloseParantheses
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

  object Atoms {
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

  lazy val TypedIdentifier =
    (
      Atoms.Identifier ~ Atoms.Colon ~ Atoms.Identifier
    ) ^^ { case name ~ _ ~ _type =>
      Trees.Identifier(name.value, Option(_type.value))
    }

  lazy val ParentethisedExpr =
    (Atoms.OpenParantheses ~> (OperatorExpr) <~ Atoms.CloseParantheses) ^^ { case a =>
      Trees.Parentensis(a)
    }

  lazy val OperatorExpr: Parser[Trees.Expr] =
    chainl1(
      ParentethisedExpr | Primitive,
      Operator ^^ { op => (l, r) =>
        Trees.OperatorExpr(l, r, op)
      }
    )

  lazy val Identifier = TypedIdentifier | Atoms.Identifier

  lazy val Operator = Atoms.Plus | Atoms.Minus | Atoms.Multiply | Atoms.Divide
  lazy val Primitive =
    Atoms.IntLit | Atoms.FloatLit | Atoms.StringLit | Atoms.Identifier;

  lazy val Assignment = Atoms.Let ~ Identifier ~ Atoms.Equal ~ Expression ^^ { case (_ ~ identifier ~ _ ~ value) =>
    Trees.Assignment(identifier, value)
  }

  lazy val Substitutions: Parser[Trees.Substitutions] = Identifier ~ rep1(Expression) ^^ { case value ~ substitutions =>
    Trees.Substitutions(value, substitutions)
  }

  lazy val BindingAssignment =
    Atoms.Let ~ repNM(
      2,
      Integer.MAX_VALUE,
      Identifier
    ) ~ Atoms.Equal ~ Expression ^^ { case (_ ~ identifier ~ _ ~ value) =>
      Trees.Assignment(
        identifier.head,
        Trees.LetBinding(identifier.tail, value)
      )
    }
  lazy val Expression =
    Substitutions | OperatorExpr

  lazy val Definition = (BindingAssignment | Assignment)
  lazy val Statement = Definition | Expression

  lazy val Program = (Statement).* ^^ { case list =>
    Trees.Program(list)
  };

  def evaluate(tokens: List[Tokens.TokenKind]) = {
    phrase(Program)(new TokenReader(tokens))
  }

}
