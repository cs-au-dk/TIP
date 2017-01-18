package tip.analysis

import tip.ast._
import tip.cfg.CfgOps._
import tip.ast.AstOps._
import tip.lattices._
import tip.ast.AstNodeData.DeclarationData

import tip.solvers._
import tip.cfg._

/**
  * Base class for the live variables analysis
  */
abstract class LiveVarsAnalysis(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData) extends FlowSensitiveAnalysis(cfg) {

  val allVars = cfg.nodes.flatMap(_.appearingIds)

  val lattice = new MapLattice(cfg.nodes, new PowersetLattice(allVars))

  def funsub(n: CfgNode, s: lattice.sublattice.Element, o: lattice.Element): lattice.sublattice.Element = {
    val succStates = n.succ.map { x =>
      o(x)
    }
    val joinState = succStates.foldLeft(lattice.sublattice.bottom) { (lub, succ) =>
      lattice.sublattice.lub(lub, succ)
    }

    n match {
      case _: CfgFunExitNode => lattice.sublattice.bottom
      case r: CfgStmtNode =>
        r.data match {
          case cond: ABinaryOp => ??? //<--- Complete here
          case ass: AAssignStmt =>
            ass.left match {
              case Left(id) => ??? //<--- Complete here
              case Right(deref) => ??? //<--- Complete here
            }
          case varr: AVarStmt => ??? //<--- Complete here
          case ret: AReturnStmt => ??? //<--- Complete here
          case out: AOutputStmt => ??? //<--- Complete here
          case _ => joinState
        }
      case _ => joinState
    }
  }
}

/**
  * Live variables analysis that uses the simple fixpoint solver.
  */
class LiveVarsAnalysisSimpleSolver(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
    extends LiveVarsAnalysis(cfg)
    with SimpleFixpointSolver
    with MapLatticeUpdateFunction[CfgNode]

/**
  * Live variables analysis that uses the worklist solver.
  */
class LiveVarsAnalysisWorklistSolver(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
    extends LiveVarsAnalysis(cfg)
    with WorklistFixpointSolver[CfgNode]
    with BackwardDependencies
