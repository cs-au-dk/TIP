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
  def edgesCallToEntry(d: DL, call: CfgCallNode, entry: CfgFunEntryNode): List[(DL, edgelattice.Element)]

  /**
    * Edges for exit-to-aftercall.
    */
  def edgesExitToAfterCall(d: DL, exit: CfgFunExitNode, aftercall: CfgAfterCallNode): List[(DL, edgelattice.Element)]

  /**
    * Edges for call-to-aftercall.
    */
  def edgesCallToAfterCall(d2: DL, call: CfgCallNode, aftercall: CfgAfterCallNode): List[(DL, edgelattice.Element)]

  /**
    * Edges for other CFG nodes.
    */
  def edgesOther(d: DL, n: CfgNode): List[(DL, edgelattice.Element)]
}
