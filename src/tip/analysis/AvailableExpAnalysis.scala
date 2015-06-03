package tip.analysis

import tip.ast._
import tip.graph._
import tip.lattices.{ReversePowersetLattice, MapLattice}
import tip.solvers.{WorklistFixpointSolver, MapLatticeUpdateFunction, SimpleFixpointSolver}

/**
 * Base class for available expressions analysis
 */
abstract class AvailableExpAnalysis(cfg: ControlFlowGraph[AstNode]) extends FlowSensitiveAnalysis(cfg) {

  import tip.graph.NodeOps._
  import tip.ast.AstOps._

  val allExps: Set[UnlabelledNode[AExpr]] = cfg.nodes.
    map(_.appearingExpressions.map(UnlabelledNode[AExpr])).flatten

  // Analysis does not accept pointers.
  require(allExps.forall(x => !x.n.isInstanceOf[AUnaryOp]))

  val lattice = new MapLattice(cfg.nodes, new ReversePowersetLattice(allExps))

  def funsub(n: GNode[AstNode], s: lattice.sublattice.Element, o: lattice.Element): lattice.sublattice.Element = {
    val predStates = n.pred.map { x => o(x) }
    val joinState: Set[UnlabelledNode[AExpr]] = predStates.foldLeft(lattice.sublattice.bottom) { (lub, pred) => lattice.sublattice.lub(lub, pred) }

    n match {
      case entry: FunEntry[AstNode] => Set()
      case r: GRealNode[AstNode] =>
        r.data match {
          case ass: AAssignStmt => ass.left match {
            case id: AIdentifier =>
              (joinState union ass.right.appearingExpressions.map(UnlabelledNode[AExpr])).
                filter { e => !(id.appearingIds subsetOf e.n.appearingIds) }
            case un: AUnaryOp => ???
          }
          case exp: AExpr =>
            joinState union exp.appearingExpressions.map(UnlabelledNode[AExpr])
          case out: AoutputStmt =>
            joinState union out.value.appearingExpressions.map(UnlabelledNode[AExpr])
          case ret: AReturnStmt =>
            joinState union ret.value.appearingExpressions.map(UnlabelledNode[AExpr])
          case _ => joinState
        }
      case _ => joinState
    }
  }
}

/**
 * Available expressions analysis that uses the simple fipoint solver.
 */
class AvailableExpAnalysisSimpleSolver(cfg: ControlFlowGraph[AstNode])
  extends AvailableExpAnalysis(cfg) with SimpleFixpointSolver with MapLatticeUpdateFunction[GNode[AstNode]]

/**
 * Available expressions analysis that uses the worklist solver.
 */
class AvailableExpAnalysisWorklistSolver(cfg: ControlFlowGraph[AstNode])
  extends AvailableExpAnalysis(cfg) with WorklistFixpointSolver[GNode[AstNode]] with ForwardDependencies