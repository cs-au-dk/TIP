package tip.solvers

import tip.ast._
import tip.cfg._
import tip.lattices._
import tip.analysis.FlowSensitiveAnalysis

import scala.collection.mutable
import tip.ast.AstNodeData.DeclarationData

/**
  * The special item representing the empty set in IDE.
  */
final case class Lambda()

/**
  * Base trait for IDE analyses.
  * @tparam D the type of items
  * @tparam L the type of the value lattice
  */
trait IDEAnalysis[D, L <: Lattice] {

  val cfg: InterproceduralProgramCfg

  type DL = Either[D, Lambda]

  /**
    * The value lattice.
    */
  val valuelattice: L

  /**
    * The edge lattice.
    */
  val edgelattice: EdgeLattice[valuelattice.type]

  /**
    * Edges for call-to-entry.
    */
  def edgesCallToEntry(d: DL, call: CfgCallNode, entry: CfgFunEntryNode): List[(DL, edgelattice.Edge)]

  /**
    * Edges for exit-to-aftercall.
    */
  def edgesExitToAfterCall(d: DL, exit: CfgFunExitNode, aftercall: CfgAfterCallNode): List[(DL, edgelattice.Edge)]

  /**
    * Edges for call-to-aftercall.
    */
  def edgesCallToAfterCall(d2: DL, call: CfgCallNode, aftercall: CfgAfterCallNode): List[(DL, edgelattice.Edge)]

  /**
    * Edges for other CFG nodes.
    */
  def edgesOther(d: DL, n: CfgNode): List[(DL, edgelattice.Edge)]
}

/**
  * Phase 1 of the IDE algorithm.
  */
abstract class IDEPhase1Analysis[D, L <: Lattice](val cfg: InterproceduralProgramCfg)(implicit val declData: DeclarationData)
    extends FlowSensitiveAnalysis[(CfgNode, Either[D, Lambda], Either[D, Lambda])](cfg)
    with IDEAnalysis[D, L]
    with WorklistFixpointPropagationFunctions[(CfgNode, Either[D, Lambda], Either[D, Lambda])] {

  /**
    * The analysis lattice.
    */
  val lattice: MapLattice[(CfgNode, DL, DL), edgelattice.type] = new MapLattice({ _: (CfgNode, DL, DL) =>
    true
  }, edgelattice)

  /**
    * The current lattice element.
    */
  var x: lattice.Element = _

  /**
    * callJumpCache(funentry, d1, call)(d3) returns the composition of the edges (call.funentry, d3) -> (call, *) -> (funentry, d1).
    * Allows faster lookup than scanning through the current lattice element.
    */
  private val callJumpCache = mutable.Map[(CfgFunEntryNode, DL, CfgCallNode), mutable.Map[DL, edgelattice.Edge]]()

  /**
    * exitJumpCache(funentry, d1) contains d2 if there is a non-bottom edge (funentry, d1) -> (funentry.exit, d2).
    * Allows faster lookup than scanning through the current lattice element.
    */
  private val exitJumpCache = mutable.Map[(CfgFunEntryNode, DL), mutable.Set[DL]]()

  import edgelattice.{Edge, IdEdge}

  val first: Set[(CfgNode, DL, DL)] = Set((cfg.funEntries(cfg.program.mainFunction), Right(Lambda()), Right(Lambda())))

  val init = IdEdge()

  /**
    * Joins the given edge into the call jump cache.
    */
  private def storeCallJump(funentry: CfgFunEntryNode, d1: DL, call: CfgCallNode, e: Edge, d3: DL) = {
    val m = callJumpCache.getOrElseUpdate((funentry, d1, call), mutable.Map[DL, edgelattice.Edge]())
    m += d3 -> m.getOrElse(d3, edgelattice.bottom).joinWith(e)
  }

  /**
    * Adds the given item to the exit jump cache.
    */
  private def storeExitJump(funentry: CfgFunEntryNode, d1: DL, d2: DL) =
    exitJumpCache.getOrElseUpdate((funentry, d1), mutable.Set[DL]()) += d2

  /**
    * Models flow from function exit to aftercall node.
    * @param d1 item at the entry of the function containing the function exit
    * @param d2 item at the function exit
    * @param funexit the function exit node
    * @param aftercall the aftercall node
    */
  private def returnflow(d1: DL, d2: DL, funexit: CfgFunExitNode, aftercall: CfgAfterCallNode): Unit = {
    import cfg._
    callJumpCache.getOrElseUpdate((funexit.entry, d1, aftercall.callNode), mutable.Map[DL, edgelattice.Edge]()).foreach {
      case (d3, e12) => // d3 is now an item at the caller function entry, and e12 is the composed edge to d1 at the callee entry
        val e3 = x(funexit, d1, d2) // summary edge from d1 to d2 at the callee function
        val e123 = e3.composeWith(e12)
        edgesExitToAfterCall(d2, funexit, aftercall).foreach {
          case (d4, e4) => // d4 is now an item at the aftercall node, and e4 is the edge from the function exit to the aftercall node
            val e = e4.composeWith(e123) // e is now the composed edge from e3 at the caller entry to d4 at the aftercall node
            propagate(e, (aftercall, d3, d4))
        }
    }
  }

  def process(nab: (CfgNode, DL, DL)) = {
    import cfg._
    nab match {
      case (n, d1, d2) =>
        NoPointers.assertContainsNode(n.data)
        val e1 = x(nab) // e1 is the composed edge from item d1 at the entry of the function containing node n to item d2 at n
        n match {

          // function call nodes
          case call: CfgCallNode =>
            call.callees.foreach { entry =>
              edgesCallToEntry(d2, call, entry).foreach {
                case (d3, e2) =>
                  // propagate to function entry
                  propagate(IdEdge(), (entry, d3, d3))
                  // cache the composed edge from the entry of the caller to the entry of the callee
                  storeCallJump(cfg.enclosingFunctionEntry(call), d3, call, e2.composeWith(e1), d1)
                  // propagate existing return flow to the after-call node
                  exitJumpCache.getOrElseUpdate((entry, d3), mutable.Set[DL]()).foreach { d4 =>
                    returnflow(d3, d4, entry.exit, call.afterCallNode)
                  }
              }
            }
            // propagate bypassing local variables to after-call
            edgesCallToAfterCall(d2, call, call.afterCallNode).foreach {
              case (d3, e2) =>
                propagate(e2.composeWith(e1), (call.afterCallNode, d1, d3))
            }

          // function exit nodes
          case funexit: CfgFunExitNode =>
            funexit.callersAfterCall.foreach { aftercall =>
              returnflow(d1, d2, funexit, aftercall)
            }
            storeExitJump(funexit.entry, d1, d2)

          // other nodes
          case _ =>
            edgesOther(d2, n).foreach {
              case (d3, e2) =>
                val e3 = e2.composeWith(e1)
                n.succ.foreach { m =>
                  propagate(e3, (m, d1, d3))
                }
            }
        }
    }
  }

  /**
    * Extracts the function summaries from the analysis result.
    * @return a map s such that s(f)(d1)(d2) is the transfer function for function f from d1 at function entry to d2 at function exit
    */
  def summaries(): mutable.Map[AFunDeclaration, mutable.Map[DL, mutable.Map[DL, Edge]]] = {
    import edgelattice.Edge
    import mutable.Map
    val res = Map[AFunDeclaration, Map[DL, Map[DL, Edge]]]()
    x.foreach {
      case ((n, d1, d2), e) =>
        n match {
          case funexit: CfgFunExitNode =>
            val m1 = res.getOrElseUpdate(funexit.data, Map[DL, Map[DL, Edge]]().withDefaultValue(Map[DL, Edge]()))
            val m2 = m1.getOrElseUpdate(d1, Map[DL, Edge]())
            m2 += d2 -> e
          case _ => // ignore other node kinds
        }
    }
    FixpointSolvers.log.verb(s"Function summaries:\n${res.map {
      case (f, s) => s"  function $f:\n${s.map { case (d1, m) => s"${m.map { case (d2, e) => s"    ($d1,$d2): $e" }.mkString("\n")}" }.mkString("\n")}"
    }.mkString("\n")} ")
    res
  }
}

/**
  * Phase 2 of the IDE algorithm.
  * Performs a forward dataflow analysis using the decomposed lattice and the micro-transformers.
  * The original RHS version of IDE uses jump functions for all nodes, not only at exits, but the analysis result and complexity is the same.
  */
class IDEPhase2Analysis[D, L <: Lattice](val cfg: InterproceduralProgramCfg, val phase1: IDEPhase1Analysis[D, L])(implicit val declData: DeclarationData)
    extends FlowSensitiveAnalysis[(CfgNode, Either[D, Lambda])](cfg)
    with WorklistFixpointPropagationFunctions[(CfgNode, Either[D, Lambda])] {

  import phase1._
  import edgelattice.Edge

  /**
    * Function summaries from phase 1.
    * Built when first invoked.
    */
  lazy val summaries: mutable.Map[AFunDeclaration, mutable.Map[DL, mutable.Map[DL, Edge]]] = phase1.summaries()

  /**
    * The analysis lattice.
    */
  val lattice: MapLattice[(CfgNode, DL), valuelattice.type] = new MapLattice({ _: (CfgNode, DL) =>
    true
  }, valuelattice)

  /**
    * The current lattice element.
    */
  var x: lattice.Element = _

  val first: Set[(CfgNode, DL)] = Set((cfg.funEntries(cfg.program.mainFunction), Right(Lambda())))

  val init: lattice.sublattice.Element = lattice.sublattice.top

  def process(nd: (CfgNode, DL)) = {
    import cfg._
    val xnd = x(nd)
    nd match {
      case (n, d) =>
        NoPointers.assertContainsNode(n.data)
        n match {

          // function call nodes
          case call: CfgCallNode =>
            call.callees.foreach { entry =>
              edgesCallToEntry(d, call, entry).foreach {
                case (d2, e) =>
                  // propagate to function entry
                  propagate(e(xnd), (entry, d2))
                  // propagate to after-call, via the function summary and exit edges
                  summaries(entry.data)(d2).foreach {
                    case (d3, e2) =>
                      edgesExitToAfterCall(d3, entry.exit, call.afterCallNode).foreach {
                        case (d4, e3) =>
                          propagate(e3(e2(e(xnd))), (call.afterCallNode, d4))
                      }
                  }
              }
            }
            // propagate bypassing local variables to after-call
            edgesCallToAfterCall(d, call, call.afterCallNode).foreach {
              case (d2, e) =>
                propagate(e(xnd), (call.afterCallNode, d2))
            }

          // function exit nodes
          case funexit: CfgFunExitNode => // ignore, return flow is handled at the call nodes

          // all other nodes, just use the micro-transformer edges
          case _ =>
            edgesOther(d, n).foreach {
              case (d2, e) =>
                n.succ.foreach { m =>
                  propagate(e(xnd), (m, d2))
                }
            }
        }
    }
  }

  val restructedlattice = new MapLattice(cfg.nodes, new MapLattice({ _: D =>
    true
  }, lattice.sublattice))

  /**
    * Restructures the analysis output to match `restructedlattice`.
    */
  def restructure(y: lattice.Element): restructedlattice.Element =
    y.foldLeft(Map[CfgNode, Map[D, lattice.sublattice.Element]]()) {
        case (acc, ((n, dl), e)) =>
          dl match {
            case Left(d) => acc + (n -> (acc.getOrElse(n, Map[D, lattice.sublattice.Element]()) + (d -> e)))
            case _ => acc // FIXME: use lifted lattice and map this to unreachable?
          }
      }
      .asInstanceOf[restructedlattice.Element] // FIXME: avoid this asInstanceOf
}
