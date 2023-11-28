// import scala.util.parsing.combinator.Parsers
// import scala.util.parsing.input.{NoPosition, Position, Reader}
// import scala.util.parsing.input.Positional
// import scala.util.parsing.combinator.{PackratParsers, Parsers}

// // TODO research, kika på LR
// object AST extends Parsers with PackratParsers {
//   override type Elem = Token

//   class TokenReader(tokens: Seq[Token]) extends Reader[Token] {
//     override def first: Token         = tokens.head
//     override def atEnd: Boolean       = tokens.isEmpty
//     override def pos:   Position      = tokens.headOption.map(_.pos).getOrElse(NoPosition)
//     override def rest:  Reader[Token] = new TokenReader(tokens.tail)
//   }

//   def apply(tokens: Seq[Token]) = {
//     val reader = new PackratReader(new TokenReader(tokens))

//     program(reader)
//   }

//   def program: PackratParser[List[Tree]] = {
//     rep1(Expression().definition.asInstanceOf[this.PackratParser[Tree]]) 
//   }
// }
