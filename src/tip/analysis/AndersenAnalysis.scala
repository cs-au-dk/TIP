package tip.analysis

import tip.ast.{ADeclaration, DepthFirstAstVisitor, _}
import tip.solvers._
import tip.util.Log
import tip.ast.AstNodeData.{AstNodeWithDeclaration, DeclarationData}

class AndersenAnalysis(program: AProgram)(implicit declData: DeclarationData) extends DepthFirstAstVisitor[Null] with PointsToAnalysis {

  val log = Log.logger[this.type]()

  trait Target
  case class Malloc(malloc: AMalloc) extends Target {
    override def toString = s"malloc-${malloc.loc}"
  }
  case class Var(id: ADeclaration) extends Target {
    override def toString = id.toString
  }

  val solver = new CubicSolver[Target, Target]

  import AstOps._
  val allTargets = (program.appearingIds.map(Var): Set[Target]) union program.appearingMallocs.map(Malloc)

  NormalizedForPointsToAnalysis.assertContainsProgram(program)

  /**
    * Generates the constraints for the given sub-AST.
    * @param node the node for which it generates the constraints
    * @param arg unused for this visitor
    */
  override def visit(node: AstNode, arg: Null): Unit = {

    node match {
      case AAssignStmt(Left(id), malloc: AMalloc, _) => ??? //<--- Complete here
      case AAssignStmt(Left(id1), AUnaryOp(RefOp, id2: AIdentifier, _), _) => ??? //<--- Complete here
      case AAssignStmt(Left(id1), id2: AIdentifier, _) => ??? //<--- Complete here
      case AAssignStmt(Left(id1), AUnaryOp(DerefOp, id2: AIdentifier, _), _) => ??? //<--- Complete here
      case AAssignStmt(Right(AUnaryOp(_, id1: AIdentifier, _)), id2: AIdentifier, _) => ??? //<--- Complete here
      case AAssignStmt(Left(_), ANull(_), _) =>
      case AAssignStmt(Left(_), _: AAtomicExpr, _) =>
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
          case Malloc(m) => m
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
