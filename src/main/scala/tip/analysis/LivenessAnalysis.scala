package tip.analysis

import tip.graph.IntraControlFlowGraph
import tip.ast._
import tip.graph.NodeOps._
import tip.ast.AstOps._
import tip.lattices._
import tip.solvers._
import tip.graph._

abstract class LivenessAnalysis(cfg: IntraControlFlowGraph[AstNode]) {

  val allVars = cfg.nodes.map {
    _.appearingIds
  }.reduce(_.union(_))

  /**
   * The lattice for the liveness analysis
   */
  val lattice = new MapLattice(cfg.nodes, new PowersetLattice(allVars))
  
  /**
   * The domain of the map lattice
   */
  val domain = cfg.nodes
  
  def transfer(n: GNode[AstNode], s: lattice.l.Element, o: lattice.Element): lattice.l.Element = {
    lattice.l.bottom
  }
}

class LivenessAnalysisSimpleSolver(cfg: IntraControlFlowGraph[AstNode]) 
  extends LivenessAnalysis(cfg) with SimpleFixpointSolver with MapNodeTransfer[GNode[AstNode]]

class LivenessAnalysisWorklistSolver(cfg: IntraControlFlowGraph[AstNode]) 
  extends LivenessAnalysis(cfg) with WorklistFixpointSolver[GNode[AstNode]] {

  override def dep(n: GNode[AstNode]) = n.pred.toSet
}
