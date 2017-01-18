package tip.analysis

import tip.cfg._

import scala.collection.immutable.Set

/**
  * Dependency methods for worklist-based analyses.
  */
trait Dependencies[N] {

  /**
    * Outgoing dependencies. Used when propagating dataflow to successors.
    * @param n an element from the worklist
    * @return the elements that depend on the given element
    */
  def outdep(n: N): Set[N]

  /**
    * Incoming dependencies. Used when computing the join from predecessors.
    * @param n an element from the worklist
    * @return the elements that the given element depends on
    */
  def indep(n: N): Set[N]
}

/**
  * Dependency methods for forward analyses.
  */
trait ForwardDependencies extends Dependencies[CfgNode] {

  def outdep(n: CfgNode) = n.succ.toSet

  def indep(n: CfgNode) = n.pred.toSet
}

/**
  * Dependency methods for backward analyses.
  */
trait BackwardDependencies extends Dependencies[CfgNode] {

  def outdep(n: CfgNode) = n.pred.toSet

  def indep(n: CfgNode) = n.succ.toSet
}

/**
  * Variant of [[ForwardDependencies]] for interprocedural analysis.
  */
trait InterproceduralForwardDependencies extends Dependencies[CfgNode] {

  val cfg: InterproceduralProgramCfg

  import cfg._

  /**
    * Like [[ForwardDependencies.outdep]] but with call and return edges.
    * A call node has an outdep to its after-call node.
    */
  override def outdep(n: CfgNode) = {
    val intraDep = n match {
      case call: CfgCallNode => call.callees
      case exit: CfgFunExitNode => exit.callersAfterCall
      case _ => Set()
    }
    intraDep ++ n.succ.toSet
  }

  /**
    * Like [[ForwardDependencies.indep]] but returning an empty set for after-call nodes.
    */
  override def indep(n: CfgNode) = {
    n match {
      case _: CfgAfterCallNode => Set()
      case _ => n.pred.toSet
    }
  }
}
