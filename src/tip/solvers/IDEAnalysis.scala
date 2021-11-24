package tip.solvers

import tip.cfg.{CfgAfterCallNode, CfgCallNode, CfgFunEntryNode, CfgFunExitNode, CfgNode, InterproceduralProgramCfg}
import tip.lattices.{EdgeFunctionLattice, Lattice}

/**
  * The special item representing the empty element in IDE.
  */
final case class Lambda()

/**
  * Base trait for IDE analyses.
  * @tparam D the type of items
  * @tparam L the type of the value lattice
  */
trait IDEAnalysis[D, L <: Lattice] {

  val cfg: InterproceduralProgramCfg

  type DL = Either[D, Lambda]

  /**
    * The value lattice.
    */
  val valuelattice: L

  /**
    * The edge lattice.
    */
  val edgelattice: EdgeFunctionLattice[valuelattice.type]

  /**
    * Edges for call-to-entry.
    */
  def edgesCallToEntry(call: CfgCallNode, entry: CfgFunEntryNode)(d: DL): Map[DL, edgelattice.Element]

  /**
    * Edges for exit-to-aftercall.
    */
  def edgesExitToAfterCall(exit: CfgFunExitNode, aftercall: CfgAfterCallNode)(d: DL): Map[DL, edgelattice.Element]

  /**
    * Edges for call-to-aftercall.
    */
  def edgesCallToAfterCall(call: CfgCallNode, aftercall: CfgAfterCallNode)(d: DL): Map[DL, edgelattice.Element]

  /**
    * Edges for other CFG nodes.
    */
  def edgesOther(n: CfgNode)(d: DL): Map[DL, edgelattice.Element]
}
