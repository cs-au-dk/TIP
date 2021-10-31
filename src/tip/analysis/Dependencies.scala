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

  def outdep(n: CfgNode): Set[CfgNode] = n.succ.toSet

  def indep(n: CfgNode): Set[CfgNode] = n.pred.toSet
}

/**
  * Dependency methods for backward analyses.
  */
trait BackwardDependencies extends Dependencies[CfgNode] {

  def outdep(n: CfgNode): Set[CfgNode] = n.pred.toSet

  def indep(n: CfgNode): Set[CfgNode] = n.succ.toSet
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
  override def outdep(n: CfgNode): Set[CfgNode] = {
    val interDep = n match {
      case call: CfgCallNode => call.callees
      case exit: CfgFunExitNode => exit.callersAfterCall
      case _ => Set()
    }
    interDep ++ n.succ.toSet
  }

  /**
    * Like [[ForwardDependencies.indep]] but returning an empty set for after-call nodes.
    */
  override def indep(n: CfgNode): Set[CfgNode] =
    n match {
      case _: CfgAfterCallNode => Set()
      case _ => n.pred.toSet
    }
}

/**
  * Variant of [[ForwardDependencies]] for context-sensitive interprocedural analysis.
  */
trait ContextSensitiveForwardDependencies[C <: CallContext] extends Dependencies[(C, CfgNode)] {

  val cfg: InterproceduralProgramCfg

  /**
    * Like [[InterproceduralForwardDependencies.outdep]] but returning an empty set for call nodes and function exit nodes,
    * and using the same context as the given pair.
    */
  override def outdep(n: (C, CfgNode)): Set[(C, CfgNode)] =
    (n._2 match {
      case _: CfgCallNode => Set()
      case _ => n._2.succ.toSet
    }).map { d =>
      (n._1, d)
    }

  /**
    * (Not implemented as it is not used by any existing analysis.)
    */
  override def indep(n: (C, CfgNode)): Set[(C, CfgNode)] =
    ???
}
