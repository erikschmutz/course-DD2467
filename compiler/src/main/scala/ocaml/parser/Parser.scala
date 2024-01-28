/*

Program             ::= (Expression | Definition) Divider | (Expression | Definition)*

Definition          ::= TypeDef | BindingAssignments | BindingRecAssignments | Assignment

TypeOptions         ::= (OpenParantheses rep1sep(TypeOption, Comma) CloseParantheses) | repNM(0, 100, TypeOption)
TypeOption          ::= TypeParam

TypeInformation     ::= ConstrDeclarations | TypeRepresation | TypeEquation

ConstrDeclarations  ::= rep1sep(ConstrDeclaration, Line)
ConstrDeclaration   ::= ConstrDeclarationOf | ConstIdentifier
ConstrDeclarationOf ::= ConstIdentifier Of TypeExpression

TypeEquation        ::= TypeExpression

TypeRepresation     ::= OpenCurly rep1sep(FieldDecration, Semicolon) CloseCurly

TypePrimitive       ::= Identifier

TypeDecl            ::= TypePrimitive | OpenParantheses TypeFunction CloseParantheses | OpenParantheses TypeTuple CloseParantheses
TypeFunction        ::= TypeDecl LeftArrow TypeExpression
TypeTuple           ::= TypeDecl Multiply rep1sep(TypeDecl, Multiply)

TypeExpression      ::= TypeFunction | TypeTuple | TypePrimitive

FieldDecration      ::= Identifier Colon TypeExpression

TypedIdentifier     ::= Identifier Colon Identifier

OperatorExpr        ::= Term rep(AddExpr | MinusExpr | FloatAddExpr)
AddExpr             ::= Plus Term
FloatAddExpr        ::= FloatPlus Term
MinusExpr           ::= Minus Term

Term                ::= Factor rep(MultiplyExpr | DivideExpr)
MultiplyExpr        ::= Multiply Factor
DivideExpr          ::= Divide Factor

Factor              ::= ParenthesisedSubstitution | Substitutions | Primitive | (OpenParantheses OperatorExpr CloseParantheses)

Identifier          ::= TypedIdentifier | Identifier

Substitution        ::= Expression | Identifier

Substitutions       ::= Identifier rep1(Substitution)

ParenthesisedSubstitution ::= Identifier OpenParantheses rep1(Substitution) CloseParantheses

BindingAssignment   ::= Let Identifier Equal Expression

BindingAssignments  ::= Let Identifier repNM(1, Integer.MAX_VALUE, BindingAssignment) Equal Expression

BindingRecAssignments ::= Let Rec Identifier repNM(1, Integer.MAX_VALUE, BindingAssignment) Equal Expression

LetBindingExpr      ::= Assignment In Expression

IfExpr              ::= If Expression Then Expression Else Expression

FormulaExpr         ::= SimpleExpression (GreaterThan | LessThan) SimpleExpression

SimpleExpression    ::= OperatorExpr | ParentensisExpression | Primitive | Identifier

ParentensisExpression ::= OpenParantheses Expression CloseParantheses

Expression          ::= ParenthesisedSubstitution | Substitutions | IfExpr | FormulaExpr | LetBindingExpr | FunctionExpression | SimpleExpression

FunctionExpression  ::= Function (Identifier | Unit) LeftArrow Expression

Primitive           ::= IntLit | False | True | FloatLit | Unit | StringLit | Identifier | CharLit

Assignment          ::= Let Identifier Equal Expression

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
    val OpenCurly =
      accept(
        "Parentensis",
        { case Tokens.OPEN_CURLY() => Trees.Tokens.OpenCurly() }
      )
    val CloseCurly =
      accept(
        "Close Parantheses",
        { case Tokens.CLOSE_CURLY() => Trees.Tokens.CloseCurly() }
      )

    val GreaterThan =
      accept(
        "GreaterThan",
        { case Tokens.GT() => Trees.Tokens.GreaterThan() }
      )

    val Comma =
      accept(
        "Comma",
        { case Tokens.COMMA() => Trees.Tokens.Comma() }
      )

    val LessThan =
      accept(
        "LessThan",
        { case Tokens.LT() => Trees.Tokens.LessThan() }
      )

    val Divider = accept(
      "Divider",
      { case Tokens.DIVIDER() => Trees.Tokens.Divider() }
    )

    val Semicolon = accept(
      "Semicolon",
      { case Tokens.SEMICOLON() => Trees.Tokens.Semicolon() }
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

    val Rec = accept(
      "Rec",
      { case Tokens.REC() => Trees.Tokens.Rec() }
    )

    val Line = accept(
      "Line",
      { case Tokens.LINE() => Trees.Tokens.Line() }
    )

    val Type = accept(
      "Type",
      { case Tokens.TYPE() => Trees.Tokens.Type() }
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

    val Of = accept(
      "Of",
      { case Tokens.OF() => Trees.Tokens.Of() }
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
    val ConstIdentifier = accept(
      "ConstIdentifier",
      { case Tokens.CONSTR_IDENTIFIER(value) =>
        Trees.ConstIdentifier(value)
      }
    )
    val TypeParam = accept(
      "TypeParam",
      { case Tokens.TYPE_PARAM(value) =>
        Trees.Tokens.TypeParam(value)
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

  object Types {
    def TypeDef = Terminals.Type ~ TypeOptions ~ Terminals.Identifier ~ Terminals.Equal ~ TypeInformation ^^ {
      case _ ~ opts ~ identifier ~ _ ~ info => Trees.TypeDeclaration(identifier.value, info, opts)
    }

    def TypeOptions =
      (Terminals.OpenParantheses ~> rep1sep(TypeOption, Terminals.Comma) <~ Terminals.CloseParantheses) | repNM(
        0,
        100,
        TypeOption
      )

    def TypeOption = Terminals.TypeParam ^^ { case id =>
      Trees.TypeOption(id.value)
    }
    def TypeInformation = ConstrDeclarations | TypeRepresation | TypeEquation;

    def ConstrDeclarations = rep1sep(ConstrDeclaration, Terminals.Line) ^^ { case constraints =>
      Trees.TypeConstraints(constraints)
    }

    def ConstrDeclarationOf = Terminals.ConstIdentifier ~ Terminals.Of ~ TypeExpression ^^ { case value ~ _ ~ of =>
      Trees.TypeConstraint(value.value, Some(of))
    };

    def ConstrDeclaration = ConstrDeclarationOf |
      Terminals.ConstIdentifier ^^ { case value =>
        Trees.TypeConstraint(value.value, None)
      };

    def TypeEquation = TypeExpression ^^ { case expr =>
      Trees.TypeEquation(expr)
    }

    def TypeRepresation =
      Terminals.OpenCurly ~> rep1sep(FieldDecration, Terminals.Semicolon) <~ Terminals.CloseCurly ^^ { case fields =>
        Trees.TypeFieldDeclarations(fields)
      };

    def TypePrimitive = Terminals.Identifier ^^ { case identifier =>
      Trees.TypePrimitive(identifier.value)
    }

    def TypeDecl: Parser[Trees.TypeExp] =
      TypePrimitive | Terminals.OpenParantheses ~> TypeFunction <~ Terminals.CloseParantheses | Terminals.OpenParantheses ~> TypeTuple <~ Terminals.CloseParantheses;

    def TypeFunction = TypeDecl ~ Terminals.LeftArrow ~ TypeExpression ^^ { case in ~ _ ~ out =>
      Trees.TypeFunction(in, out)
    }

    def TypeTuple = TypeDecl ~ Terminals.Multiply ~
      rep1sep(TypeDecl, Terminals.Multiply) ^^ { case expr ~ _ ~ list =>
        Trees.TypeTuple(List(expr) ++ list)
      }

    def TypeExpression: Parser[Trees.TypeExp] = TypeFunction | TypeTuple | TypePrimitive;

    def FieldDecration = Terminals.Identifier ~ Terminals.Colon ~ TypeExpression ^^ { case identifier ~ _ ~ _type =>
      Trees.TypeFieldDeclaration(identifier.value, _type)
    };
  }

  object Record {
    def Field = Terminals.Identifier ~ Terminals.Equal ~ Expression ^^ { case key ~ _ ~ value =>
      Trees.RecordEntry(key.value, value)
    }

    def Record = Terminals.OpenCurly ~> rep1sep(Field, Terminals.Semicolon) <~ Terminals.CloseCurly ^^ { case entries =>
      Trees.Record(entries)
    }
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

  def Factor =
    ParenthesisedSubstitution | Substitutions | Primitive | (Terminals.OpenParantheses ~> (OperatorExpr) <~ Terminals.CloseParantheses)

  def Identifier = TypedIdentifier | Terminals.Identifier
  def Operator = Terminals.Plus | Terminals.Minus | Terminals.Multiply | Terminals.Divide | Terminals.FloatPlus
  def Primitive =
    Terminals.IntLit | Terminals.False | Terminals.True | Terminals.FloatLit | Terminals.Unit | Terminals.StringLit | Terminals.Identifier | Terminals.CharLit;

  def Assignment = Terminals.Let ~ Identifier ~ Terminals.Equal ~ Expression ^^ { case (_ ~ identifier ~ _ ~ value) =>
    Trees.Assignment(identifier, value)
  }

  def Substitution: Parser[Trees.Expr => Trees.Substitutions] =
    (Expression | Identifier) ^^ {
      case value => {
        Trees.Substitutions(_, value)
      }
    }

  def Substitutions: Parser[Trees.Expr] = Identifier ~ rep1(Substitution) ^^ {
    case id ~ substitutions => {
      (substitutions.foldLeft(id.asInstanceOf[Trees.Expr]))((acc, f) => f(acc))
    }
  }

  def ParenthesisedSubstitution: Parser[Trees.Expr] =
    Identifier ~ Terminals.OpenParantheses ~ rep1(
      Substitution
    ) ~ Terminals.CloseParantheses ^^ {
      case id ~ _ ~ substitutions ~ _ => {
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

  def BindingRecAssignments =
    Terminals.Let ~ Terminals.Rec ~ Identifier ~ repNM(
      1,
      Integer.MAX_VALUE,
      BindingAssignment
    ) ~ Terminals.Equal ~ Expression ^^ { case (_ ~ _ ~ identifier ~ lets ~ _ ~ value) =>
      Trees.Assignment(
        identifier,
        Trees.LetBindingExpr(
          identifier,
          Trees.Recursive(),
          (lets.foldRight(value))((f, acc) => f(acc))
        )
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

  def IfExpr = Terminals.If ~ Expression ~ Terminals.Then ~ Expression ~ Terminals.Else ~ Expression ^^ {
    case _ ~ expr ~ _ ~ thn ~ _ ~ els => Trees.If(expr, thn, els)
  }

  def FormulaExpr: Parser[Trees.OperatorExpr] =
    SimpleExpression ~ (Terminals.GreaterThan | Terminals.LessThan) ~ SimpleExpression ^^ {
      case left ~ operator ~ right => Trees.OperatorExpr(left, right, operator)
    }

  def SimpleExpression: Parser[Trees.Expr] =
    OperatorExpr | ParentensisExpression | Primitive | Identifier

  def ParentensisExpression: Parser[Trees.Expr] = Terminals.OpenParantheses ~> Expression <~ Terminals.CloseParantheses
  def Expression: Parser[Trees.Expr] =
    Record.Record |
      ParenthesisedSubstitution | Substitutions | IfExpr | FormulaExpr | LetBindingExpr | FunctionExpression | SimpleExpression;

  def Definition = (Types.TypeDef | BindingAssignments | BindingRecAssignments | Assignment)
  // Identifier
  def Program = (((Expression | Definition) <~ Terminals.Divider) | (Expression | Definition)).* ^^ { case list =>
    Trees.Program(list)
  };

  def evaluate(tokens: List[Tokens.TokenKind]) = {
    phrase(Program)(new TokenReader(tokens))
  }

}
