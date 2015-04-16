package tip.ast

/**
 * A depth-first visitor of the AST
 *
 */
trait DepthFirstAstVisitor[A] {

  def visit(node: AstNode, arg: A)

  /**
   * Recursively perform the visit to the sub-node of the passed node, passing the provided argument
   *
   * @param node the node whose children need to be visited
   * @param arg the argument to be passed to the visit of the children
   */
  def visitChildren(node: AstNode, arg: A): Unit = {
    node match {
      case call: ACallFuncExpr =>
        visit(call.targetFun, arg)
        call.args.foreach(visit(_, arg))
      case bin: ABinaryOp =>
        visit(bin.left, arg)
        visit(bin.right, arg)
      case un: AUnaryOp =>
        visit(un.target, arg)
      case ass: AAssignStmt =>
        visit(ass.right, arg)
        visit(ass.left, arg)
      case block: ABlockStmt =>
        block.content.foreach(visit(_, arg))
      case iff: AIfStmt =>
        visit(iff.guard, arg)
        visit(iff.ifBranch, arg)
        iff.elseBranch.map(visit(_, arg))
      case out: AoutputStmt =>
        visit(out.value, arg)
      case ret: AReturnStmt =>
        visit(ret.value, arg)
      case varr: AVarStmt =>
        varr.declIds.foreach(visit(_, arg))
      case whl: AWhileStmt =>
        visit(whl.guard, arg)
        visit(whl.innerBlock, arg)
      case funDec: AFunDeclaration =>
        visit(funDec.name, arg)
        funDec.args.foreach(visit(_, arg))
        visit(funDec.stmts, arg)
      case p: AProgram =>
        p.fun.foreach(visit(_, arg))
      case atom: AstAtom =>
    }
  }
}
