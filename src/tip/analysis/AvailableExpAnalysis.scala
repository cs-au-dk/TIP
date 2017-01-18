package tip.analysis

import tip.ast._
import tip.cfg._
import tip.lattices.{ReversePowersetLattice, MapLattice}
import tip.solvers.{WorklistFixpointSolver, MapLatticeUpdateFunction, SimpleFixpointSolver}
import tip.ast.AstNodeData.DeclarationData

/**
  * Base class for available expressions analysis
  */
abstract class AvailableExpAnalysis(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData) extends FlowSensitiveAnalysis(cfg) {

  import tip.cfg.CfgOps._
  import tip.ast.AstOps._

  val allExps: Set[UnlabelledNode[AExpr]] = cfg.nodes.flatMap(_.appearingExpressions.map(UnlabelledNode[AExpr]))

  // Analysis does not accept pointers.
  NoPointers.assertContainsProgram(cfg.prog)

  val lattice = new MapLattice(cfg.nodes, new ReversePowersetLattice(allExps))

  def funsub(n: CfgNode, s: lattice.sublattice.Element, o: lattice.Element): lattice.sublattice.Element = {
    val predStates = n.pred.map { x =>
      o(x)
    }
    val joinState: Set[UnlabelledNode[AExpr]] = predStates.foldLeft(lattice.sublattice.bottom) { (lub, pred) =>
      lattice.sublattice.lub(lub, pred)
    }

    n match {
      case _: CfgFunEntryNode => Set()
      case r: CfgStmtNode =>
        r.data match {
          case ass: AAssignStmt =>
            ass.left match {
              case Left(id) =>
                (joinState union ass.right.appearingExpressions.map(UnlabelledNode[AExpr])).filter { e =>
                  !(id.appearingIds subsetOf e.n.appearingIds)
                }
              case Right(_) => ???
            }
          case exp: AExpr =>
            joinState union exp.appearingExpressions.map(UnlabelledNode[AExpr])
          case out: AOutputStmt =>
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
class AvailableExpAnalysisSimpleSolver(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
    extends AvailableExpAnalysis(cfg)
    with SimpleFixpointSolver
    with MapLatticeUpdateFunction[CfgNode]

/**
  * Available expressions analysis that uses the worklist solver.
  */
class AvailableExpAnalysisWorklistSolver(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
    extends AvailableExpAnalysis(cfg)
    with WorklistFixpointSolver[CfgNode]
    with ForwardDependencies
