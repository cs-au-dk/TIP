package tip.analysis

import tip.ast._
import tip.cfg._
import tip.lattices._
import tip.solvers._
import tip.ast.AstNodeData.{AstNodeWithDeclaration, DeclarationData}

/**
  * The base class for interval analysis.
  */
abstract class IntervalAnalysis(cfg: FragmentCfg)(implicit declData: DeclarationData)
    extends FlowSensitiveAnalysis[CfgNode](cfg)
    with MapLatticeSolver[CfgNode] {

  import tip.cfg.CfgOps._

  val declaredVars = cfg.nodes.flatMap(_.declaredVars)

  val lattice = new MapLattice(cfg.nodes, new LiftLattice(new MapLattice(declaredVars, new IntervalLattice())))

  def transferUnlifted(n: CfgNode, s: lattice.sublattice.sublattice.Element): lattice.sublattice.sublattice.Element = {
    n match {
      case r: CfgStmtNode =>
        r.data match {
          case varr: AVarStmt =>
            varr.declIds.foldLeft(s) { (acc, id) =>
              acc + (id -> lattice.sublattice.sublattice.sublattice.FullInterval)
            }
          case ass: AAssignStmt =>
            ass.left match {
              case Left(id) =>
                val vdef = id.declaration
                s + (vdef -> absEval(ass.right, s))
              case Right(_) => ???
            }
          case _ => s
        }
      case _ => s
    }
  }

  override def funsub(n: CfgNode, x: lattice.Element): lattice.sublattice.Element = {
    import lattice.sublattice._
    n match {
      // function entry nodes are always reachable
      case funentry: CfgFunEntryNode => lift(lattice.sublattice.sublattice.bottom)
      // all other nodes are processed with join+transfer
      case _ => super.funsub(n, x)
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
      case id: AIdentifier => env(id.declaration)
      case num: ANumber => (lattice.sublattice.sublattice.sublattice.IntNum(num.value), lattice.sublattice.sublattice.sublattice.IntNum(num.value))
      case bin: ABinaryOp =>
        val left = absEval(bin.left, env)
        val right = absEval(bin.right, env)
        bin.operator match {
          case Eqq => lattice.sublattice.sublattice.sublattice.eqq(left, right)
          case GreatThan => lattice.sublattice.sublattice.sublattice.gt(left, right)
          case Divide => lattice.sublattice.sublattice.sublattice.div(left, right)
          case Minus => lattice.sublattice.sublattice.sublattice.minus(left, right)
          case Plus => lattice.sublattice.sublattice.sublattice.plus(left, right)
          case Times => lattice.sublattice.sublattice.sublattice.times(left, right)
          case _ => ???
        }
      case input: AInput => lattice.sublattice.sublattice.sublattice.FullInterval
      case _ => ???
    }
  }
}

/**
  * Interval analysis, using the worklist solver with init and widening.
  */
class IntervalAnalysisWorklistSolverWithInit(cfg: ProgramCfg)(implicit declData: DeclarationData)
    extends IntervalAnalysis(cfg)
    with WorklistFixpointSolverWithInit[CfgNode]
    with ForwardDependencies {

  val first = cfg.funEntries.values.toSet[CfgNode]
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

  val narrowingSteps = 5
}
