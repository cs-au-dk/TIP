package tip.solvers

import tip.analysis.{FlowSensitiveAnalysis, ForwardDependencies}
import tip.ast.AstNodeData.DeclarationData
import tip.cfg._
import tip.lattices.{EdgeEnvLattice, Lattice, LiftLattice, MapLattice}

import scala.collection.immutable.Set

/**
  * A tabulation solver, variant of IDESolver.
  * @tparam D the type of items
  * @tparam L the type of the value lattice
  */
abstract class SummarySolver[D, L <: Lattice](val cfg: InterproceduralProgramCfg)(implicit declData: DeclarationData)
    extends FlowSensitiveAnalysis(false)
    with IDEAnalysis[D, L] {

  class Phase1(val cfg: InterproceduralProgramCfg) extends WorklistFixpointPropagationSolver[CfgNode] with ForwardDependencies {

    val statelattice = new EdgeEnvLattice[D, valuelattice.type, edgelattice.type](edgelattice)

    val lattice = new MapLattice[CfgNode, LiftLattice[statelattice.type]](new LiftLattice(statelattice))

    import cfg._
    import lattice.sublattice.{lift, Lift}
    import edgelattice.IdEdge

    /**
      * Initialize worklist with the program entry point and the identity function.
      */
    val first = Set(programEntry)

    override val init = lift(Map((Right(Lambda()), Right(Lambda())) -> IdEdge()))

    import statelattice._

    /**
      * Transfer functions.
      */
    override def transferUnlifted(n: CfgNode, s: Element): Element =
      n match {
        case call: CfgCallNode =>
          for (entry <- call.callees) {
            val s2 = propagateReturn(entry.exit, call.afterCallNode)
            val s3 = s2.map { case ((_, d), _) => (d, d) -> IdEdge() }
            propagate(s3, entry)
          }
          compose2(s, edgesCallToAfterCall(call, call.afterCallNode))
        case funexit: CfgFunExitNode =>
          for (aftercall <- funexit.callersAfterCall)
            propagateReturn(funexit, aftercall)
          bottom // no successors for this kind of node, but we have to return something
        case _ =>
          compose2(s, edgesOther(n))
      }

    private def propagateReturn(funexit: CfgFunExitNode, aftercall: CfgAfterCallNode): Element =
      x(aftercall.callNode) match {
        case Lift(s1) =>
          val s2 = compose2(s1, edgesCallToEntry(aftercall.callNode, funexit.entry))
          x(funexit) match {
            case Lift(s) =>
              val s3 = compose(s2, s)
              val s4 = compose2(s3, edgesExitToAfterCall(funexit, aftercall))
              propagate(lift(s4), aftercall)
            case _ =>
          }
          s2
        case _ =>
          statelattice.bottom
      }
  }

  class Phase2(val cfg: InterproceduralProgramCfg, val phase1: Phase1) extends WorklistFixpointPropagationFunctions[CfgNode] {

    val statelattice = new MapLattice[D, valuelattice.type](valuelattice)

    val lattice = new MapLattice[CfgNode, statelattice.type](statelattice)

    import cfg._
    import phase1.lattice.sublattice._

    var x: lattice.Element = _

    val first = Set(programEntry)

    val init = statelattice.bottom

    def process(n: CfgNode): Unit = {
      val s = x(n)
      n match {
        case funentry: CfgFunEntryNode =>
          for (call <- callsInFunction(funentry))
            propagate(sublattice.apply(unlift(phase1.x(call)), s), call)
        case call: CfgCallNode =>
          for (funentry <- call.callees)
            propagate(sublattice.apply(edgesCallToEntry(call, funentry) _, s), funentry)
        case _ =>
      }
    }

    override def analyze(): lattice.Element = {
      super.analyze()
      nodes.foreach {
        case _: CfgFunEntryNode | _: CfgCallNode => // already processed
        case n =>
          x += n -> sublattice.apply(unlift(phase1.x(n)), x(enclosingFunctionEntry(n)))
      }
      x
    }
  }

  def analyze(): Map[CfgNode, Map[D, valuelattice.Element]] = {
    FixpointSolvers.log.verb(s"Summary pre-analysis")
    val p1 = new Phase1(cfg)
    val res1 = p1.analyze()
    FixpointSolvers.log.verb(s"Result from pre-analysis:\n  ${res1.mkString("\n  ")}")
    FixpointSolvers.log.verb(s"Main analysis")
    val p2 = new Phase2(cfg, p1)
    val res2 = p2.analyze()
    FixpointSolvers.log.verb(s"Result from main analysis:\n  ${res2.mkString("\n  ")}")
    res2
  }
}
