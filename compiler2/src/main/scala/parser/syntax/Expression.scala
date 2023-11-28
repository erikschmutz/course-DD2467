case class OperationTree(left: Tree, right: Tree, operator: Token)
    extends Tree {}


case class Expression() extends SyntaxDefinition {
  override type Output = Tree

  def definition = 
    (t(Expression) ~ t(BinaryPrim) ~ t(Expression) ^^ {case a ~ b ~ c => OperationTree(a, c, b)}) | 
    t(Constant) 
}
