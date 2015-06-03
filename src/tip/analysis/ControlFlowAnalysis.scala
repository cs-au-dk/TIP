package tip.analysis

import tip.ast.{AAssignStmt, AIdentifier, AIdentifierDeclaration, AProgram, AstNode, AstOps, DepthFirstAstVisitor, PointerNormalised, _}
import tip.logging.Log
import tip.solvers.CubicSolver

class ControlFlowAnalysis(program: AProgram)
  extends DepthFirstAstVisitor[Null] {

  val log = Log.typeLogger[this.type](Log.Level.Info)

  case class Decl(fun: AFunDeclaration) {
    override def toString = s"Decl(${fun.name}:${fun.offset})"
  }

  case class AstVariable(n: AstNode) {
    override def toString = n match {
      case fun: AFunDeclaration => s"Var(${fun.name}:${fun.offset})"
      case _ => s"Var($n:${n.offset})"
    }
  }

  val solver = new CubicSolver[Decl, AstVariable]

  val allFunctions = program.fun.toSet

  performAnalysis()

  /**
   * Generates the constraints for the given sub-AST.
   * @param node the node for which it generates the constraints
   * @param arg unused for this visitor
   */
  override def visit(node: AstNode, arg: Null) {

    PointerNormalised.checkAstLanguageRestriction(node)

    /**
     * Get the declaration if the supplied AstNode is an identifier, 
     * which might be a variable declaration or a function declaration.
     * It returns the node itself, otherwise.
     */
    def decl(n:AstNode): AstNode = n match {
      case id: AIdentifier => id.meta.definition.getOrElse(id)
      case _ => n
    }

    node match {
      case fun: AFunDeclaration =>
        solver.addConstantConstraint(Decl(fun), AstVariable(fun))
      case AAssignStmt(id1: AIdentifier, e, _) =>
        solver.addSubsetConstraint(AstVariable(decl(e)), AstVariable(decl(id1)))
      case ACallFuncExpr(id2: AIdentifier, args, _) =>
        decl(id2) match {
          case fun: AFunDeclaration => ??? //<---- Complete here
          case id: AIdentifier => //<---- Complete here
          case _ => ???
        }
      case _ =>
    }
    visitChildren(node, null)
  }

  private def performAnalysis() {
    visit(program, null)
    log.info(s"Solution is:\n$solver")
  }
}