case class ConstantTree(token: Token)
    extends Tree {}

case class Constant() extends SyntaxDefinition {
    override type Output = ConstantTree
    
    def definition = t(IntLiteral) ^^ {
        ConstantTree(_)
    }
}