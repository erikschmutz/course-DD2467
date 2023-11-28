case class BinaryPrimTree(token: Token)
    extends Tree {}


case class BinaryPrim() extends SyntaxDefinition {
    override type Output = Token

    def definition = (Plus() | Minus())
}