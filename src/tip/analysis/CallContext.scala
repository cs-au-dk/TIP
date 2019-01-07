package tip.analysis

import tip.cfg.{CfgCallNode, CfgFunEntryNode}
import tip.lattices.Lattice

/**
  * Base trait for call contexts.
  */
trait CallContext

/**
  * Functions for creating call contexts.
  * @tparam C the type for call contexts
  */
trait CallContextFunctions[C <: CallContext] {

  /**
    * The type for the abstract state lattice.
    */
  type statelatticetype = Lattice

  val statelattice: statelatticetype

  /**
    * Initial context, for the main function.
    */
  def initialContext: C

  /**
    * Makes a context for the callee at a function call site.
    * @param c the caller context
    * @param n the current call node
    * @param x the callee entry abstract state
    * @param f the callee function
    * @return the context for the callee
    */
  def makeCallContext(c: C, n: CfgCallNode, x: statelattice.Element, f: CfgFunEntryNode): C
}

/**
  * Call context for call strings.
  */
case class CallStringContext(cs: List[CfgCallNode]) extends CallContext {

  /**
    * Creates string representation using the source locations of the calls in the call string.
    */
  override def toString: String = cs.map { _.data.loc } mkString ("[", ",", "]")
}

/**
  * Call context construction for call strings.
  */
trait CallStringFunctions extends CallContextFunctions[CallStringContext] {

  /**
    * Default maximum length for call strings: 1.
    */
  val maxCallStringLength = 1

  /**
    * Creates a context as the empty list.
    */
  def initialContext: CallStringContext = CallStringContext(Nil)

  /**
    * Creates a context as the singleton list consisting of the call node (and ignoring the other arguments).
    */
  def makeCallContext(c: CallStringContext, n: CfgCallNode, x: statelattice.Element, f: CfgFunEntryNode): CallStringContext =
    CallStringContext((n :: c.cs).slice(0, maxCallStringLength))
}

/**
  * Call context for functional approach.
  * @param x a lattice element
  */
case class FunctionalContext(x: Any) extends CallContext { // TODO: find some way to make Scala type check that x is indeed of type statelattice.Element

  override def toString: String = x.toString
}

/**
  * Call context construction for functional approach.
  */
trait FunctionalFunctions extends CallContextFunctions[FunctionalContext] {

  /**
    * Creates a context as the empty abstract state.
    */
  def initialContext = FunctionalContext(statelattice.bottom)

  /**
    * Creates a context as the singleton list consisting of the call node (and ignoring the other arguments).
    */
  def makeCallContext(c: FunctionalContext, n: CfgCallNode, x: statelattice.Element, f: CfgFunEntryNode): FunctionalContext =
    FunctionalContext(x)
}
