package tip.analysis

import tip.graph.ControlFlowGraph
import tip.ast._
import tip.graph.NodeOps._
import tip.ast.AstOps._
import tip.lattices._
import tip.solvers._
import tip.graph._

/**
 * Base class for the live variables analysis
 */
abstract class LiveVarsAnalysis(cfg: ControlFlowGraph[AstNode]) extends FlowSensitiveAnalysis(cfg) {

  val allVars = cfg.nodes.map {
    _.appearingIds
  }.flatten

  val lattice = new MapLattice(cfg.nodes, new PowersetLattice(allVars))
  
  def funsub(n: GNode[AstNode], s: lattice.sublattice.Element, o: lattice.Element): lattice.sublattice.Element = {
    val succStates = n.succ.map { x => o(x) }
    val joinState = succStates.foldLeft(lattice.sublattice.bottom) { (lub, succ) => lattice.sublattice.lub(lub, succ) }

    n match {
      case exit: FunExit[AstNode] => lattice.sublattice.bottom
      case r: GRealNode[AstNode] =>
        r.data match {
          //case .... <--- Complete here
          case _ => joinState
        }
      case _ => joinState
    }
  }
}

/**
 * Live variables analysis that uses the simple fixpoint solver.
 */
class LiveVarsAnalysisSimpleSolver(cfg: ControlFlowGraph[AstNode])
  extends LiveVarsAnalysis(cfg) with SimpleFixpointSolver with MapLatticeUpdateFunction[GNode[AstNode]]

/**
 * Live variables analysis that uses the worklist solver.
 */
class LiveVarsAnalysisWorklistSolver(cfg: ControlFlowGraph[AstNode])
  extends LiveVarsAnalysis(cfg) with WorklistFixpointSolver[GNode[AstNode]] with BackwardDependencies
