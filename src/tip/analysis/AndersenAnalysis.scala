package tip.analysis

import tip.ast.{ADeclaration, DepthFirstAstVisitor, _}
import tip.solvers._
import tip.util.Log
import tip.ast.AstNodeData.{AstNodeWithDeclaration, DeclarationData}

class AndersenAnalysis(program: AProgram)(implicit declData: DeclarationData) extends DepthFirstAstVisitor[Null] with PointsToAnalysis {

  val log = Log.logger[this.type]()

  trait Target
  case class Alloc(alloc: AAlloc) extends Target {
    override def toString = s"alloc-${alloc.loc}"
  }
  case class Var(id: ADeclaration) extends Target {
    override def toString = id.toString
  }

  val solver = new CubicSolver[Target, Target]

  import AstOps._
  val allTargets = (program.appearingIds.map(Var): Set[Target]) union program.appearingAllocs.map(Alloc)

  NormalizedForPointsToAnalysis.assertContainsProgram(program)

  /**
    * Generates the constraints for the given sub-AST.
    * @param node the node for which it generates the constraints
    * @param arg unused for this visitor
    */
  override def visit(node: AstNode, arg: Null): Unit = {

    node match {
      case AAssignStmt(id: AIdentifier, alloc: AAlloc, _) => ??? //<--- Complete here
      case AAssignStmt(id1: AIdentifier, AUnaryOp(RefOp, id2: AIdentifier, _), _) => ??? //<--- Complete here
      case AAssignStmt(id1: AIdentifier, id2: AIdentifier, _) => ??? //<--- Complete here
      case AAssignStmt(id1: AIdentifier, AUnaryOp(DerefOp, id2: AIdentifier, _), _) => ??? //<--- Complete here
      case AAssignStmt(AUnaryOp(_, id1: AIdentifier, _), id2: AIdentifier, _) => ??? //<--- Complete here
      case AAssignStmt(_: AIdentifier, ANull(_), _) =>
      case AAssignStmt(_: AIdentifier, _: AAtomicExpr, _) =>
      case ass: AAssignStmt => NormalizedForPointsToAnalysis.LanguageRestrictionViolation(s"Assignment $ass not expected")
      case _ =>
    }
    visitChildren(node, null)
  }

  /**
    * @inheritdoc
    */
  def pointsTo() = {
    val pointsTo = solver.getSolution.collect {
      case (v: Var, ts: Set[Target]) =>
        v.id -> ts.map {
          case Var(x) => x
          case Alloc(m) => m
        }
    }
    println(s"Points-to:\n${pointsTo.mapValues(v => s"{${v.mkString(",")}}").mkString("\n")}")
    pointsTo
  }

  /**
    * @inheritdoc
    */
  def mayAlias(): (ADeclaration, ADeclaration) => Boolean = { (x: ADeclaration, y: ADeclaration) =>
    solver.getSolution(Var(x)).intersect(solver.getSolution(Var(y))).nonEmpty
  }

  /**
    * @inheritdoc
    */
  override def analyze() = {
    visit(program, null)
  }
}
