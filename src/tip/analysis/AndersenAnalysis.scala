package tip.analysis

import tip.ast.DepthFirstAstVisitor
import tip.logging.Log
import tip.solvers._
import tip.ast._
import scala.collection.{ mutable, immutable }

class AndersenAnalysis(program: AProgram)
  extends DepthFirstAstVisitor[Null] {

  val log = Log.typeLogger[this.type](Log.Level.Info)

  trait Target
  case class Malloc(malloc: AMalloc) extends Target {
    override def toString() = s"malloc-${malloc.offset}"
  }
  case class Var(id: AIdentifierDeclaration) extends Target {
    override def toString() = s"$id:${id.offset}"
  }

  val solver = new CubicSolver[Target, Target]

  import AstOps._
  val allTargets = (program.appearingIds.map(Var(_)): Set[Target]) union program.appearingMallocs.map(Malloc(_))

  performAnalysis()

  /**
   * Generates the constraints for the given sub-AST.
   * @param node the node for which it generates the constraints
   * @param arg unused for this visitor
   */
  override def visit(node: AstNode, arg: Null): Unit = {
    PointerNormalised.checkAstLanguageRestriction(node)

    /**
     * Get the (variable or function) declaration of the identifier.
     */
    def decl(id:AIdentifier): AIdentifierDeclaration = id.meta.definition.get.asInstanceOf[AIdentifierDeclaration]

    node match {
      case AAssignStmt(id: AIdentifier, malloc: AMalloc, _) =>
        //<---- Complete here
      case AAssignStmt(id1: AIdentifier, AUnaryOp(r: RefOp, id2: AIdentifier, _ ), _) =>
        //<---- Complete here
      case AAssignStmt(id1: AIdentifier, id2: AIdentifier, _) =>
        //<---- Complete here
      case AAssignStmt(id1: AIdentifier, AUnaryOp(d: DerefOp, id2: AIdentifier, _ ), _) =>
        //<---- Complete here
      case AAssignStmt(AUnaryOp(d: DerefOp, id1: AIdentifier, _), id2: AIdentifier, _) =>
        //<---- Complete here
      case AAssignStmt(id: AIdentifier, ANull(_), _) =>
      case AAssignStmt(id: AIdentifier, atom: AstAtom, _) =>
      case ass: AAssignStmt => PointerNormalised.LanguageRestrictionViolation(ass)
      case _ =>
    }
    visitChildren(node, null)
  }

  private def performAnalysis(): Unit = {
    visit(program, null)
    log.info(s"Solution is:\n$solver")
  }
}
