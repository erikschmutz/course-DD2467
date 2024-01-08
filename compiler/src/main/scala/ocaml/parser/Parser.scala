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

    val GreaterThan =
      accept(
        "GreaterThan",
        { case Tokens.GT() => Trees.Tokens.GreaterThan() }
      )

    val LessThan =
      accept(
        "LessThan",
        { case Tokens.GT() => Trees.Tokens.LessThan() }
      )

    val Divider = accept(
      "Divider",
      { case Tokens.DIVIDER() => Trees.Tokens.Divider() }
    )

    val IntLit =
      accept(
        "Int",
        { case Tokens.INT_LIT(value) => Trees.Tokens.IntLit(value) }
      )

    val StringLit =
      accept(
        "StringLit",
        { case Tokens.STR_LIT(value) => Trees.Tokens.StringLit(value) }
      )
    val CharLit =
      accept(
        "Int",
        { case Tokens.CHAR_LIT(value) => Trees.Tokens.CharLit(value) }
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

    val FloatPlus = accept(
      "DivideFloat",
      { case Tokens.FLOAT_PLUS() => Trees.Tokens.FloatPlus() }
    )

    val Function = accept(
      "DivideFloat",
      { case Tokens.FUNCTION() => Trees.Tokens.Function() }
    )

    val LeftArrow = accept(
      "LeftArrow",
      { case Tokens.ARROW() => Trees.Tokens.LeftArrow() }
    )

    val Unit = accept(
      "Unit",
      { case Tokens.UNIT() => Trees.Tokens.Unit() }
    )

    val Minus = accept(
      "Minus",
      { case Tokens.MINUS() => Trees.Tokens.Minus() }
    )
    val Let = accept(
      "Let",
      { case Tokens.LET() => Trees.Tokens.Let() }
    )

    val In = accept(
      "In",
      { case Tokens.IN() => Trees.Tokens.In() }
    )

    val If = accept(
      "If",
      { case Tokens.IF() => Trees.Tokens.If() }
    )
    val True = accept(
      "True",
      { case Tokens.TRUE() => Trees.Tokens.Bool(true) }
    )

    val False = accept(
      "False",
      { case Tokens.FALSE() => Trees.Tokens.Bool(false) }
    )

    val Then = accept(
      "Then",
      { case Tokens.THEN() => Trees.Tokens.Then() }
    )

    val Else = accept(
      "Else",
      { case Tokens.ELSE() => Trees.Tokens.Else() }
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

  def OperatorExpr: Parser[Trees.Expr] = Term ~ rep(AddExpr | MinusExpr | FloatAddExpr) ^^ { case a ~ b =>
    (a /: b)((acc, f) => f(acc))
  }

  def AddExpr = (Terminals.Plus ~> Term) ^^ { case a =>
    Trees.OperatorExpr(_, a, Trees.Tokens.Plus())
  }

  def FloatAddExpr = (Terminals.FloatPlus ~> Term) ^^ { case a =>
    Trees.OperatorExpr(_, a, Trees.Tokens.FloatPlus())
  }

  def MinusExpr = (Terminals.Minus ~> Term) ^^ { case a =>
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
  def Operator = Terminals.Plus | Terminals.Minus | Terminals.Multiply | Terminals.Divide | Terminals.FloatPlus
  def Primitive =
    Terminals.IntLit | Terminals.False | Terminals.True | Terminals.FloatLit | Terminals.Unit | Terminals.StringLit | Terminals.Identifier | Terminals.CharLit;

  def Assignment = Terminals.Let ~ Identifier ~ Terminals.Equal ~ Expression ^^ { case (_ ~ identifier ~ _ ~ value) =>
    Trees.Assignment(identifier, value)
  }
// f 10

  def Substitution: Parser[Trees.Expr => Trees.Substitutions] =
    (Identifier | Expression) ^^ {
      case value => {
        Trees.Substitutions(_, value)
      }
    }

  def ParenthesisedSubstitution: Parser[Trees.Expr => Trees.Substitutions] =
    (Terminals.OpenParantheses ~> Expression <~ Terminals.CloseParantheses) ^^ {
      case value => {
        Trees.Substitutions(value, _)
      }
    }

  def Substitutions: Parser[Trees.Expr] = Identifier ~ rep1(Substitution) ^^ {
    case id ~ substitutions => {
      (substitutions.foldLeft(id.asInstanceOf[Trees.Expr]))((acc, f) => f(acc))
    }
  }

  def BindingAssignment: Parser[Trees.Expr => Trees.LetBinding] = (Identifier) ^^ {
    case value => {
      Trees.LetBinding(value, _)
    }
  }

  def FunctionExpression =
    Terminals.Function ~> (Identifier | Terminals.Unit) ~ Terminals.LeftArrow ~ Expression ^^ {
      case (arg ~ _ ~ body) => {
        arg match {
          case arg: Trees.Identifier  => Trees.LetBinding(arg, body)
          case arg: Trees.Tokens.Unit => Trees.LetBinding(Trees.Identifier("a", Some("unit")), body)
        }
      }
    }

  def BindingAssignments =
    Terminals.Let ~ Identifier ~ repNM(
      1,
      Integer.MAX_VALUE,
      BindingAssignment
    ) ~ Terminals.Equal ~ Expression ^^ { case (_ ~ identifier ~ lets ~ _ ~ value) =>
      Trees.Assignment(
        identifier,
        (lets.foldRight(value))((f, acc) => f(acc))
      )
    }

  def LetBindingExpr =
    Assignment ~ Terminals.In ~ Expression ^^ { case (ass ~ _ ~ expr) =>
      Trees.LetBindingExpr(
        ass.variable,
        ass.exprs,
        expr
      )
    }

  def IfExpr = log(Terminals.If ~ Expression ~ Terminals.Then ~ Expression ~ Terminals.Else ~ Expression ^^ {
    case _ ~ expr ~ _ ~ thn ~ _ ~ els => Trees.If(expr, thn, els)
  })("IFExpr");

  def FormulaExpr = SimpleExpression ~ (Terminals.GreaterThan | Terminals.LessThan) ~ SimpleExpression ^^ {
    case left ~ operator ~ right => Trees.OperatorExpr(left, right, operator)
  }

  def SimpleExpression = Substitutions | OperatorExpr | ParentensisExpression

  def ParentensisExpression: Parser[Trees.Expr] = Terminals.OpenParantheses ~> Expression <~ Terminals.CloseParantheses
  def Expression: Parser[Trees.Expr] =
    IfExpr | FormulaExpr | LetBindingExpr | FunctionExpression | Substitutions | OperatorExpr | ParentensisExpression
  def Definition = (BindingAssignments | Assignment)
  def Program = (((Expression | Definition) <~ Terminals.Divider) | (Expression | Definition)).* ^^ { case list =>
    Trees.Program(list)
  };

  def evaluate(tokens: List[Tokens.TokenKind]) = {
    phrase(Program)(new TokenReader(tokens))
  }

}
