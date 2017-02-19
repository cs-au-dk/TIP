package tip.analysis

import tip.ast._
import tip.cfg._
import tip.lattices.{ReversePowersetLattice, MapLattice}
import tip.solvers.{SimpleWorklistFixpointSolver, SimpleMapLatticeFixpointSolver}
import tip.ast.AstNodeData.DeclarationData

/**
  * Base class for available expressions analysis
  */
abstract class AvailableExpAnalysis(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData) extends FlowSensitiveAnalysis[CfgNode](cfg) {

  import tip.cfg.CfgOps._
  import tip.ast.AstOps._

  val allExps: Set[UnlabelledNode[AExpr]] = cfg.nodes.flatMap(_.appearingExpressions.map(UnlabelledNode[AExpr]))

  // Analysis does not accept pointers.
  NoPointers.assertContainsProgram(cfg.prog)

  val lattice = new MapLattice(cfg.nodes, new ReversePowersetLattice(allExps))

  def transfer(n: CfgNode, s: lattice.sublattice.Element): lattice.sublattice.Element = {
    n match {
      case _: CfgFunEntryNode => Set()
      case r: CfgStmtNode =>
        r.data match {
          case ass: AAssignStmt =>
            ass.left match {
              case Left(id) =>
                (s union ass.right.appearingExpressions.map(UnlabelledNode[AExpr])).filter { e =>
                  !(id.appearingIds subsetOf e.n.appearingIds)
                }
              case Right(_) => ???
            }
          case exp: AExpr =>
            s union exp.appearingExpressions.map(UnlabelledNode[AExpr])
          case out: AOutputStmt =>
            s union out.value.appearingExpressions.map(UnlabelledNode[AExpr])
          case ret: AReturnStmt =>
            s union ret.value.appearingExpressions.map(UnlabelledNode[AExpr])
          case _ => s
        }
      case _ => s
    }
  }
}

/**
  * Available expressions analysis that uses the simple fipoint solver.
  */
class AvailableExpAnalysisSimpleSolver(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
    extends AvailableExpAnalysis(cfg)
    with SimpleMapLatticeFixpointSolver[CfgNode]
    with ForwardDependencies

/**
  * Available expressions analysis that uses the worklist solver.
  */
class AvailableExpAnalysisWorklistSolver(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
    extends AvailableExpAnalysis(cfg)
    with SimpleWorklistFixpointSolver[CfgNode]
    with ForwardDependencies
