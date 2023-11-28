// import scala.util.parsing.combinator.Parsers
// import scala.util.parsing.input.{NoPosition, Position, Reader}
// import scala.util.parsing.input.Positional
// import scala.util.parsing.combinator.{PackratParsers, Parsers}

// trait Tree extends Positional

// object AST {
//   case class IntTree(value: Integer) extends Tree

//   abstract class Operator extends Tree
//   case class PlusTree() extends Operator
//   case class MinusTree() extends Operator

//   case class TestTree() extends Tree
//   case class OperationTree(left: Tree, right: Tree, op: Operator) extends Tree
// }

// object Syntax extends Parsers {
//   override type Elem = Token

//   def IntTree =
//     accept("literal", { case IntLiteral(value) => AST.IntTree(value) })

//   def PlusTree =
//     Plus() ^^ { case _ =>
//       AST.PlusTree()
//     }

//   def MinusTree =
//     Minus() ^^ { case _ =>
//       AST.MinusTree()
//     }

//   def Operator: Parser[AST.Operator] = PlusTree | MinusTree;

//   def OperationTree: Parser[AST.OperationTree] = positioned {
//     (IntTree ~ Operator ~ Expression) ^^ { case a ~ operator ~ b =>
//       AST.OperationTree(a, b, operator)
//     }
//   }

//   def Expression = 
//     OperationTree | IntTree

//   def program = {
//     phrase(rep(Expression))
//   }

// }

// object SyntaxParser extends Parsers with PackratParsers {
//   override type Elem = Token
//   class TokenReader(tokens: Seq[Token]) extends Reader[Token] {
//     override def first: Token = tokens.head
//     override def atEnd: Boolean = tokens.isEmpty
//     override def pos: Position =
//       tokens.headOption.map(_.pos).getOrElse(NoPosition)
//     override def rest: Reader[Token] = new TokenReader(tokens.tail)
//   }

//   def parse(tokens: List[Token]) = {
//     println("Tokens:")
//     println(tokens)
//     val reader = new TokenReader(tokens)
//     val result = Syntax.program(reader)
//     println("Result:")
//     println(result)
//   }
// }
