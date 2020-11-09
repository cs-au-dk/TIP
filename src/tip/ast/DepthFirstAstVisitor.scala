package tip.ast

/**
  * A depth-first visitor for ASTs.
  * @tparam A argument type
  */
trait DepthFirstAstVisitor[A] {

  def visit(node: AstNode, arg: A): Unit

  /**
    * Recursively perform the visit to the sub-node of the passed node, passing the provided argument.
    *
    * @param node the node whose children need to be visited
    * @param arg the argument to be passed to all sub-nodes
    */
  def visitChildren(node: AstNode, arg: A): Unit =
    node match {
      case call: ACallFuncExpr =>
        visit(call.targetFun, arg)
        call.args.foreach(visit(_, arg))
      case bin: ABinaryOp =>
        visit(bin.left, arg)
        visit(bin.right, arg)
      case un: AUnaryOp =>
        visit(un.subexp, arg)
      case as: AAssignStmt =>
        as.left match {
          case id: AIdentifier =>
            visit(id, arg)
          case dw: ADerefWrite =>
            visit(dw.exp, arg)
          case dfw: ADirectFieldWrite =>
            visit(dfw.id, arg)
          case ifw: AIndirectFieldWrite =>
            visit(ifw.exp, arg)
        }
        visit(as.right, arg)
      case block: ABlock =>
        block.body.foreach(visit(_, arg))
      case iff: AIfStmt =>
        visit(iff.guard, arg)
        visit(iff.ifBranch, arg)
        iff.elseBranch.foreach(visit(_, arg))
      case out: AOutputStmt =>
        visit(out.exp, arg)
      case ret: AReturnStmt =>
        visit(ret.exp, arg)
      case err: AErrorStmt =>
        visit(err.exp, arg)
      case varr: AVarStmt =>
        varr.declIds.foreach(visit(_, arg))
      case whl: AWhileStmt =>
        visit(whl.guard, arg)
        visit(whl.innerBlock, arg)
      case funDec: AFunDeclaration =>
        funDec.params.foreach(visit(_, arg))
        visit(funDec.stmts, arg)
      case p: AProgram =>
        p.funs.foreach(visit(_, arg))
      case acc: AFieldAccess =>
        visit(acc.record, arg)
      case rec: ARecord =>
        rec.fields.foreach(f => visit(f.exp, arg))
      case alloc: AAlloc =>
        visit(alloc.exp, arg)
      case ref: AVarRef =>
        visit(ref.id, arg)
      case _: AAtomicExpr | _: AIdentifierDeclaration =>
    }
}
