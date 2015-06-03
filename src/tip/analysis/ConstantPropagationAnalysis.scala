package tip.analysis

import tip.ast._
import tip.graph.{AuxNode, GRealNode, GNode, ControlFlowGraph}
import tip.lattices.{FlatLattice, ReversePowersetLattice, PowersetLattice, MapLattice}
import tip.solvers.{WorklistFixpointSolver, MapLatticeUpdateFunction, SimpleFixpointSolver}

abstract class ConstantPropagationAnalysis(cfg: ControlFlowGraph[AstNode]) extends FlowSensitiveAnalysis(cfg) {

  import tip.graph.NodeOps._
  import tip.ast.AstOps._

  val declaredVars = cfg.nodes.map {
    _.declaredIds
  }.flatten

  val lattice = new MapLattice(cfg.nodes, new MapLattice(declaredVars, new FlatLattice[Int]()))

  def funsub(n: GNode[AstNode], s: lattice.sublattice.Element, o: lattice.Element): lattice.sublattice.Element = {
    val predStates = n.pred.map { x => o(x) }
    val joinState = predStates.foldLeft(lattice.sublattice.bottom) { (lub, pred) => lattice.sublattice.lub(lub, pred) }
    n match {
      case r: GRealNode[AstNode] =>
        r.data match {
          case varr: AVarStmt =>
            varr.declIds.foldLeft(joinState) { (lub, id) =>
              lub + (id -> lattice.sublattice.sublattice.Top())
            }
          case ass: AAssignStmt =>
            ass.left match {
              case id: AIdentifier =>
                val vdef = id.meta.definition match {
                  case Some(x: AIdentifier) => x
                  case Some(y) => ???
                  case _ => throw new IllegalArgumentException(s"No definition found for $id")
                }
                joinState + (vdef -> absEval(ass.right, joinState))
              case _: AUnaryOp => ???
            }
          case _ => joinState
        }
      case _ => joinState
    }
  }

  private def absEval(exp: AExpr, env: Map[AIdentifier, lattice.sublattice.sublattice.Element]): lattice.sublattice.sublattice.Element = {
    exp match {
      case bin: ABinaryOp =>
        val left = absEval(bin.left, env)
        val right = absEval(bin.right, env)
        (left, right) match {
          case (lattice.sublattice.sublattice.FlatEl(x), lattice.sublattice.sublattice.FlatEl(y)) =>
            bin.operator match {
              case Eqq() =>  lattice.sublattice.sublattice.FlatEl(if (x == y) 1 else 0)
              case Divide() => lattice.sublattice.sublattice.FlatEl(x / y)
              case GreatThan() => lattice.sublattice.sublattice.FlatEl(if (x > y) 1 else 0)
              case Minus() => lattice.sublattice.sublattice.FlatEl(x - y)
              case Plus() => lattice.sublattice.sublattice.FlatEl(x + y)
              case Times() => lattice.sublattice.sublattice.FlatEl(x * y)
              case _ => ???
            }
          case (lattice.sublattice.sublattice.Bot(), _) => lattice.sublattice.sublattice.Bot()
          case (_, lattice.sublattice.sublattice.Bot()) => lattice.sublattice.sublattice.Bot()
          case (_, lattice.sublattice.sublattice.Top()) => lattice.sublattice.sublattice.Top()
          case (lattice.sublattice.sublattice.Top(), _) => lattice.sublattice.sublattice.Top()
        }
      case id: AIdentifier => 
        val defId = id.meta.definition match {
          case Some(x: AIdentifier) => x
          case _ => ???
        }
        env(defId)
      case input: AInput => lattice.sublattice.sublattice.Top()
      case num: ANumber => lattice.sublattice.sublattice.FlatEl(num.value)
      case _ => ???

    }
  }
}

/**
 * Constant propagation analysis that uses the simple fipoint solver.
 */
class ConstantPropagationAnalysisSimpleSolver(cfg: ControlFlowGraph[AstNode])
  extends ConstantPropagationAnalysis(cfg) with SimpleFixpointSolver with MapLatticeUpdateFunction[GNode[AstNode]]

/**
 * Constant propagation analysis that uses the worklist solver.
 */
class ConstantPropagationAnalysisWorklistSolver(cfg: ControlFlowGraph[AstNode])
  extends ConstantPropagationAnalysis(cfg) with WorklistFixpointSolver[GNode[AstNode]] with ForwardDependencies