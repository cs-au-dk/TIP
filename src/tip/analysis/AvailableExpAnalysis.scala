package tip.analysis

import tip.ast._
import tip.cfg._
import tip.lattices.{MapLattice, ReversePowersetLattice}
import tip.solvers.{SimpleMapLatticeFixpointSolver, SimpleWorklistFixpointSolver}
import tip.ast.AstNodeData.DeclarationData

import scala.collection.immutable.Set

/**
  * Base class for available expressions analysis.
  */
abstract class AvailableExpAnalysis(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData) extends FlowSensitiveAnalysis(true) {

  import tip.cfg.CfgOps._
  import tip.ast.AstOps._

  val allExps: Set[UnlabelledNode[AExpr]] = cfg.nodes.flatMap(_.appearingNonInputExpressions.map(UnlabelledNode[AExpr]))

  val lattice: MapLattice[CfgNode, ReversePowersetLattice[UnlabelledNode[AExpr]]] = new MapLattice(new ReversePowersetLattice(allExps))

  val domain: Set[CfgNode] = cfg.nodes

  NoPointers.assertContainsProgram(cfg.prog)
  NoRecords.assertContainsProgram(cfg.prog)

  def transfer(n: CfgNode, s: lattice.sublattice.Element): lattice.sublattice.Element =
    n match {
      case _: CfgFunEntryNode => Set()
      case r: CfgStmtNode =>
        r.data match {
          case as: AAssignStmt =>
            as.left match {
              case id: AIdentifier =>
                (s union as.right.appearingNonInputExpressions.map(UnlabelledNode[AExpr])).filter { e =>
                  !(id.appearingIds subsetOf e.n.appearingIds)
                }
              case _ => ???
            }
          case exp: AExpr =>
            s union exp.appearingNonInputExpressions.map(UnlabelledNode[AExpr])
          case out: AOutputStmt =>
            s union out.exp.appearingNonInputExpressions.map(UnlabelledNode[AExpr])
          case ret: AReturnStmt =>
            s union ret.exp.appearingNonInputExpressions.map(UnlabelledNode[AExpr])
          case _ => s
        }
      case _ => s
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
