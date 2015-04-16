package tip.analysis

import tip.ast._
import tip.lattices._
import tip.graph._
import tip.solvers._
import tip.graph.NodeOps._

/**
 * Base class for intra-procedural sign analysis.
 */
abstract class IntraprocSignAnalysis(cfg: IntraControlFlowGraph[AstNode]) {

  type Sign = SignLattice.SignElement.Value

  import SignLattice.SignElement._

  val allVars = cfg.nodes.map {
    _.appearingIds
  }.reduce(_.union(_))

  val declaredVars = cfg.nodes.map {
    _.declaredIds
  }.reduce(_.union(_))

  // We require that there are no spurious variables appearing in the cfg,
  // e.g. function parameters
  require(allVars == declaredVars)

  /**
   * The lattice for the analysis, which is the lattice of maps associating with 
   * each node a state, where the state is a map associating with each declared variable
   * an element of the sign lattice.
   */
  val lattice = new MapLattice(cfg.nodes, new MapLattice(declaredVars, SignLattice))

  /**
   * The domain of the map lattice
   */
  val domain = cfg.nodes

  /**
   * The transfer function.
   */
  def transfer(n: GNode[AstNode], s: lattice.l.Element, o: lattice.Element): lattice.l.Element = {
    val predStates = n.pred.map { x => o(x) }
    val joinState = predStates.foldLeft(lattice.l.bottom) { (lub, pred) => lattice.l.lub(lub, pred) }

    n match {
      case r: GRealNode[AstNode] =>
        r.data match {
          // <---- Complete here
          case _ => joinState
        }
      case aux: AuxNode[AstNode] => joinState
    }
  }

  private def unsupportedError(x: AstNode) = throw new IllegalArgumentException(s"Sign analysis meant to be run on programs without $x")

  /**
   * Returns the sign of i.
   */
  private def sign(i: Int): Sign = {
    if (i == 0)
      Zero
    else if (i > 0)
      Pos
    else
      Neg
  }

  /**
   * Evaluate the expression exp in the abstract domain of signs,
   * assuming env is the current environment.
   */
  private def eval[A](env: Map[AIdentifier, Sign], exp: AExpr): Sign = {
    exp match {
      case id: AIdentifier =>
        val defId = id.meta.definition match {
          case Some(x: AIdentifier) => x
          case _ => unsupportedError(id)
        }
        env(defId)
      case intc: ANumber => sign(intc.value)
      case bin: ABinaryOp =>
        bin.operator match {
          case _: Plus =>
            abs(absPlus, eval(env, bin.left), eval(env, bin.right))
          case _: Minus =>
            abs(absMinus, eval(env, bin.left), eval(env, bin.right))
          case _: Times =>
            abs(absTimes, eval(env, bin.left), eval(env, bin.right))
          case _: Divide =>
            abs(absDivide, eval(env, bin.left), eval(env, bin.right))
          case _: GreatThan =>
            abs(absGT, eval(env, bin.left), eval(env, bin.right))
          case _: Eqq =>
            abs(absEq, eval(env, bin.left), eval(env, bin.right))
          case _ => unsupportedError(bin)
        }
      case _ => unsupportedError(exp)
    }
  }
 
  private val signValues = Map(Bottom -> 0, Zero -> 1, Neg -> 2, Pos -> 3, Top -> 4)

  private def abs(op: List[List[Sign]], x: Sign, y: Sign): Sign = {
    op(signValues(x))(signValues(y))
  }

  private val absPlus = List(
    List(Bottom, Bottom, Bottom, Bottom, Bottom),
    List(Bottom, Zero, Neg, Pos, Top),
    List(Bottom, Neg, Neg, Top, Top),
    List(Bottom, Pos, Top, Pos, Top),
    List(Bottom, Top, Top, Top, Top))

  private val absMinus = List(
    List(Bottom, Bottom, Bottom, Bottom, Bottom),
    List(Bottom, Zero, Pos, Neg, Top),
    List(Bottom, Neg, Top, Neg, Top),
    List(Bottom, Pos, Pos, Top, Top),
    List(Bottom, Top, Top, Top, Top))

  private val absTimes = List(
    List(Bottom, Zero, Bottom, Bottom, Bottom),
    List(Zero, Zero, Zero, Zero, Zero),
    List(Bottom, Zero, Pos, Neg, Top),
    List(Bottom, Zero, Neg, Pos, Top),
    List(Bottom, Zero, Top, Top, Top))

  private val absDivide = List(
    List(Bottom, Bottom, Bottom, Bottom, Bottom),
    List(Bottom, Top, Zero, Zero, Top),
    List(Bottom, Top, Top, Top, Top),
    List(Bottom, Top, Top, Top, Top))

  private val absGT = List(
    List(Bottom, Bottom, Bottom, Bottom, Bottom),
    List(Bottom, Zero, Pos, Zero, Top),
    List(Bottom, Zero, Top, Zero, Top),
    List(Bottom, Pos, Pos, Top, Top),
    List(Bottom, Top, Top, Top, Top))

  private val absEq = List(
    List(Bottom, Bottom, Bottom, Bottom, Bottom),
    List(Bottom, Pos, Zero, Zero, Top),
    List(Bottom, Zero, Top, Zero, Top),
    List(Bottom, Zero, Zero, Top, Top),
    List(Bottom, Top, Top, Top, Top))
}

/**
 * A sign analysis that uses [[tip.solvers.SimpleFixpointSolver]].
 */
class IntraprocSignAnalysisSimpleSolver(cfg: IntraControlFlowGraph[AstNode]) 
  extends IntraprocSignAnalysis(cfg) with SimpleFixpointSolver with MapNodeTransfer[GNode[AstNode]]

/**
 * A sign analysis that uses [[tip.solvers.WorklistFixpointSolver]].
 */
class IntraprocSignAnalysisWorklistSolver(cfg: IntraControlFlowGraph[AstNode]) 
  extends IntraprocSignAnalysis(cfg) with WorklistFixpointSolver[GNode[AstNode]] {

  override def dep(n: GNode[AstNode]) = n.succ.toSet
}