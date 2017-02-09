package tip.analysis

import tip.ast._
import tip.cfg._
import tip.lattices._
import tip.solvers._
import tip.ast.AstNodeData.{AstNodeWithDeclaration, DeclarationData}

/**
  * The base class for interval analysis.
  */
abstract class IntervalAnalysis(cfg: FragmentCfg)(implicit declData: DeclarationData) extends FlowSensitiveAnalysis(cfg) {

  import tip.cfg.CfgOps._

  val declaredVars = cfg.nodes.flatMap(_.declaredVars)

  val lattice = new MapLattice(cfg.nodes, new LiftLattice(new MapLattice(declaredVars, new IntervalLattice())))

  def funsub(n: CfgNode, s: lattice.sublattice.Element, o: lattice.Element): lattice.sublattice.Element = {
    import lattice.sublattice._
    val predStates = n.pred.map { x =>
      o(x)
    }
    // Whenever the transfer is called, the program point becomes "reached",
    // so compute the join state starting from the *sub-lattice* bottom element
    val reached: lattice.sublattice.Element = lattice.sublattice.sublattice.bottom
    val joinState = predStates.foldLeft(reached) { (lub, pred) =>
      lattice.sublattice.lub(lub, pred)
    }

    n match {
      case r: CfgStmtNode =>
        r.data match {
          case ass: AAssignStmt =>
            ass.left match {
              case Left(id) =>
                val vdef = id.declaration
                joinState + (vdef -> absEval(ass.right, joinState))
              case Right(_) => ???
            }
          case _ => joinState
        }
      case _ => joinState
    }
  }

  /**
    * The abstract evaluation function for an expression.
    * @param exp the expression
    * @param env the current abstract environment
    * @return the result of the evaluation
    */
  private def absEval(exp: AExpr,
                      env: Map[ADeclaration, lattice.sublattice.sublattice.sublattice.Element]): lattice.sublattice.sublattice.sublattice.Element = {
    exp match {
      case bin: ABinaryOp =>
        val left = absEval(bin.left, env)
        val right = absEval(bin.right, env)
        bin.operator match {
          case Eqq => lattice.sublattice.sublattice.sublattice.eqq(left, right)
          case GreatThan => lattice.sublattice.sublattice.sublattice.gt(left, right)
          case Divide => lattice.sublattice.sublattice.sublattice.div(left, right)
          case Minus => lattice.sublattice.sublattice.sublattice.sub(left, right)
          case Plus => lattice.sublattice.sublattice.sublattice.sum(left, right)
          case Times => lattice.sublattice.sublattice.sublattice.prod(left, right)
          case _ => ???
        }
      case id: AIdentifier =>
        val defId = id.declaration
        env(defId)
      case _: AInput => lattice.sublattice.sublattice.sublattice.FullInterval
      case num: ANumber =>
        (lattice.sublattice.sublattice.sublattice.IntNum(num.value), lattice.sublattice.sublattice.sublattice.IntNum(num.value))
      case _ => ???
    }
  }
}

/**
  * Interval analysis, using the worklist solver with init and widening.
  */
class IntervalAnalysisWorklistSolverWithWidening(cfg: ProgramCfg)(implicit declData: DeclarationData)
    extends IntervalAnalysis(cfg)
    with WorklistFixpointSolverWithInitAndSimpleWidening[CfgNode]
    with ForwardDependencies {

  import tip.cfg.CfgOps._

  val first = cfg.funEntries.values.toSet[CfgNode]

  /**
    * Int values occurring in the program, plus -infinity and +infinity.
    */
  private val B = cfg.nodes.flatMap { n =>
    {
      import lattice.sublattice.sublattice.sublattice._
      n.appearingConstants.map { x =>
        IntNum(x.value): Num
      } + MInf + PInf
    }
  }

  def backedge(src: CfgNode, dst: CfgNode): Boolean = cfg.rank(src) > cfg.rank(dst)

  private def minB(b: lattice.sublattice.sublattice.sublattice.Num) = B.filter(b <= _).min

  private def maxB(a: lattice.sublattice.sublattice.sublattice.Num) = B.filter(_ <= a).max

  def widen(s: lattice.sublattice.Element): lattice.sublattice.Element = {
    import lattice.sublattice.sublattice.sublattice._
    import lattice.sublattice._
    s match {
      case lattice.sublattice.Bottom => s
      case lattice.sublattice.Lift(m) => ??? //<--- Complete here
    }
  }
}

/**
  * Interval analysis, using the worklist solver with init, widening, and narrowing.
  */
class IntervalAnalysisWorklistSolverWithWideningAndNarrowing(cfg: ProgramCfg)(implicit declData: DeclarationData)
    extends IntervalAnalysisWorklistSolverWithWidening(cfg)
    with WorklistFixpointSolverWithInitAndSimpleWideningAndNarrowing[CfgNode] {

  val narrowingSteps = 3
}
