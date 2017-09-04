package tip.analysis

import tip.ast._
import tip.cfg.{IntraproceduralProgramCfg, CfgNode, CfgStmtNode}
import tip.lattices.{FlatLattice, MapLattice}
import tip.solvers.{SimpleMapLatticeFixpointSolver, SimpleWorklistFixpointSolver}
import tip.ast.AstNodeData.{AstNodeWithDeclaration, DeclarationData}

abstract class ConstantPropagationAnalysis(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData) extends FlowSensitiveAnalysis[CfgNode](cfg) {

  import tip.cfg.CfgOps._

  val declaredVars: Set[ADeclaration] = cfg.nodes.flatMap(_.declaredVars)

  val lattice = new MapLattice(cfg.nodes, new MapLattice(declaredVars, new FlatLattice[Int]()))

  def transfer(n: CfgNode, s: lattice.sublattice.Element): lattice.sublattice.Element = {
    n match {
      case r: CfgStmtNode =>
        r.data match {
          case varr: AVarStmt =>
            varr.declIds.foldLeft(s) { (acc, id) =>
              acc + (id -> lattice.sublattice.sublattice.Top)
            }
          case ass: AAssignStmt =>
            ass.left match {
              case id: AIdentifier =>
                val vdef: ADeclaration = id.declaration
                s + (vdef -> absEval(ass.right, s))
              case _ => ???
            }
          case _ => s
        }
      case _ => s
    }
  }

  private def absEval(exp: AExpr, env: Map[ADeclaration, lattice.sublattice.sublattice.Element]): lattice.sublattice.sublattice.Element = {
    exp match {
      case id: AIdentifier => env(id.declaration)
      case num: ANumber => lattice.sublattice.sublattice.FlatEl(num.value)
      case bin: ABinaryOp =>
        val left = absEval(bin.left, env)
        val right = absEval(bin.right, env)
        (left, right) match {
          case (lattice.sublattice.sublattice.FlatEl(x), lattice.sublattice.sublattice.FlatEl(y)) =>
            bin.operator match {
              case Eqq => lattice.sublattice.sublattice.FlatEl(if (x == y) 1 else 0)
              case Divide => if (y != 0) lattice.sublattice.sublattice.FlatEl(x / y) else lattice.sublattice.sublattice.Top
              case GreatThan => lattice.sublattice.sublattice.FlatEl(if (x > y) 1 else 0)
              case Minus => lattice.sublattice.sublattice.FlatEl(x - y)
              case Plus => lattice.sublattice.sublattice.FlatEl(x + y)
              case Times => lattice.sublattice.sublattice.FlatEl(x * y)
              case _ => ???
            }
          case (lattice.sublattice.sublattice.Bot, _) => lattice.sublattice.sublattice.Bot
          case (_, lattice.sublattice.sublattice.Bot) => lattice.sublattice.sublattice.Bot
          case (_, lattice.sublattice.sublattice.Top) => lattice.sublattice.sublattice.Top
          case (lattice.sublattice.sublattice.Top, _) => lattice.sublattice.sublattice.Top
        }
      case input: AInput => lattice.sublattice.sublattice.Top
      case _ => ???

    }
  }
}

/**
  * Constant propagation analysis that uses the simple fipoint solver.
  */
class ConstantPropagationAnalysisSimpleSolver(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
    extends ConstantPropagationAnalysis(cfg)
    with SimpleMapLatticeFixpointSolver[CfgNode]
    with ForwardDependencies

/**
  * Constant propagation analysis that uses the worklist solver.
  */
class ConstantPropagationAnalysisWorklistSolver(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
    extends ConstantPropagationAnalysis(cfg)
    with SimpleWorklistFixpointSolver[CfgNode]
    with ForwardDependencies
