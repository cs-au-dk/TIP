package tip.analysis

import tip.ast._
import tip.graph.{AuxNode, GRealNode, GNode, ControlFlowGraph}
import tip.lattices._
import tip.solvers._

/**
 * The base class for the interval analysis
 */
abstract class IntervalAnalysis(cfg: ControlFlowGraph[AstNode]) extends FlowSensitiveAnalysis(cfg) {

  import tip.graph.NodeOps._
  import tip.ast.AstOps._

  val declaredVars = cfg.nodes.map {
    _.declaredIds
  }.flatten

  val lattice = new MapLattice(cfg.nodes, new LiftLattice(new MapLattice(declaredVars, new IntervalLattice())))

  def funsub(n: GNode[AstNode], s: lattice.sublattice.Element, o: lattice.Element): lattice.sublattice.Element = {
    import lattice.sublattice._
    val predStates = n.pred.map { x => o(x) }
    // Whenever the transfer is called, the state is reached
    val reached: lattice.sublattice.Element = lattice.sublattice.sublattice.bottom
    val joinState = predStates.foldLeft(reached) { (lub, pred) => lattice.sublattice.lub(lub, pred) }
    
    n match {
      case r: GRealNode[AstNode] =>
        r.data match {
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

  /**
   * The abstract evaluation function for an expression
   * @param exp the expression
   * @param env the current abstract environment
   * @return the result of the evaluation
   */
  private def absEval(exp: AExpr, env: Map[AIdentifier, lattice.sublattice.sublattice.sublattice.Element]): lattice.sublattice.sublattice.sublattice.Element = {
    exp match {
      case bin: ABinaryOp =>
        val left = absEval(bin.left, env)
        val right = absEval(bin.right, env)
            bin.operator match {
              case Eqq() => lattice.sublattice.sublattice.sublattice.eqq(left, right)
              case GreatThan() => lattice.sublattice.sublattice.sublattice.gt(left, right)
              case Divide() => lattice.sublattice.sublattice.sublattice.div(left, right)
              case Minus() => lattice.sublattice.sublattice.sublattice.sub(left, right)
              case Plus() => lattice.sublattice.sublattice.sublattice.sum(left, right)
              case Times() => lattice.sublattice.sublattice.sublattice.prod(left, right)
              case _ => ???
            }
      case id: AIdentifier =>
        val defId = id.meta.definition match {
          case Some(x: AIdentifier) => x
          case _ => ???
        }
        env(defId)
      case input: AInput => lattice.sublattice.sublattice.sublattice.fullInterval
      case num: ANumber => (lattice.sublattice.sublattice.sublattice.IntNum(num.value), lattice.sublattice.sublattice.sublattice.IntNum(num.value))
      case _ => ???
    }
  }
}

/**
 * Interval analysis, using the worklist solver with init and widening.
 */
class IntervalAnalysisWorklistSolverWithWidening(cfg: ControlFlowGraph[AstNode])
  extends IntervalAnalysis(cfg) with WorklistFixpointSolverWithInitAndSetWidening[GNode[AstNode]] with ForwardDependencies {

  import tip.graph.NodeOps._

  /**
   * The rank of the graph, which is used to detect the backedges.
   */
  val rank = cfg.rank

  val first = cfg.entries.values.toSet

  val B = cfg.nodes.map {n => n.constants.map { x => x.value }} .flatten

  def backedge(src: GNode[AstNode], dst: GNode[AstNode]): Boolean = rank(src) > rank(dst)

  def widen(s: lattice.sublattice.Element): lattice.sublattice.Element = {
    import lattice.sublattice.sublattice.sublattice._
    import lattice.sublattice._
    s match {
      case lattice.sublattice.Bottom => s
      case lattice.sublattice.Lift(m) =>
        m.mapValues { v =>
          v match {
            case _ => emptyInterval // <----- Complete Here
          }
        }
    }
  }
}

/**
 * Interval analysis, using the worklist solver with init, widening, and narrowing.
 */
class IntervalAnalysisWorklistSolverWithWideningAndNarrowing(cfg: ControlFlowGraph[AstNode])
  extends IntervalAnalysisWorklistSolverWithWidening(cfg) with WorklistFixpointSolverWithInitAndSetWideningAndNarrowing[GNode[AstNode]] {

  val narrowingSteps = 3
}
