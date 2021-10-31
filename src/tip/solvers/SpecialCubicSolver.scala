package tip.solvers

import tip.util.Log

import scala.collection.mutable
import scala.language.implicitConversions

/**
  * Special cubic solver.
  *
  * @tparam V type of variables (tokens are a subset of variables)
  */
class SpecialCubicSolver[V] {

  private val log = Log.logger[this.type]()

  private var lastTknId: Int = -1

  private def nextTokenId: Int = {
    lastTknId += 1; lastTknId
  }

  private class Node(
    val succ: mutable.Set[V] = mutable.Set(), // note: the edges between nodes go via the variables
    val tokenSol: mutable.BitSet = new mutable.BitSet(), // the current solution bitvector
    val fromTriggers: mutable.Set[V] = mutable.Set(), // universally quantified constraints from the current node
    val toTriggers: mutable.Set[V] = mutable.Set() // universally quantified constraints to the current node
  )

  /**
    * The map from variables to nodes.
    */
  private val varToNode: mutable.Map[V, Node] = mutable.Map()

  /**
    * Provides an index for each token that we have seen.
    */
  private val tokenToInt: mutable.Map[V, Int] = mutable.Map()

  /**
    * Maps each token index to the corresponding token.
    */
  private val intToToken: mutable.ArrayBuffer[V] = mutable.ArrayBuffer()

  /**
    * Worklist of (token, variable) pairs.
    */
  private val worklist: mutable.Queue[(Int, V)] = mutable.Queue()

  /**
    * Returns the index associated with the given token.
    * Allocates a fresh index if the token hasn't been seen before.
    */
  implicit private def getTokenInt(tkn: V): Int =
    tokenToInt.getOrElseUpdate(tkn, { intToToken += tkn; nextTokenId })

  /**
    * Retrieves the node associated with the given variable.
    * Allocates a fresh node if the variable hasn't been seen before.
    */
  private def getOrMakeNode(x: V): Node =
    varToNode.getOrElseUpdate(x, new Node)

  /**
    * Adds a token to the solution for a variable.
    */
  private def addToken(t: Int, x: V): Unit =
    if (getOrMakeNode(x).tokenSol.add(t))
      worklist += ((t, x))

  /**
    * Adds an inclusion edge.
    */
  private def addEdge(x: V, y: V): Unit =
    if (x != y) {
      val nx = getOrMakeNode(x)
      if (nx.succ.add(y)) {
        getOrMakeNode(y)
        log.verb(s"Adding edge \u27E6$x\u27E7 -> \u27E6$y\u27E7")
        for (t <- nx.tokenSol)
          addToken(t, y)
      }
    }

  /**
    * Processes items in the worklist.
    */
  private def propagate(): Unit =
    while (worklist.nonEmpty) {
      // Pick element from worklist
      val (t, x) = worklist.dequeue()
      val tkn = intToToken(t)
      val nx = varToNode(x)
      // Process quantified constraints
      for (y <- nx.fromTriggers)
        addEdge(tkn, y)
      for (y <- nx.toTriggers)
        addEdge(y, tkn)
      // Propagate token to successors
      for (v <- nx.succ)
        addToken(t, v)
    }

  /**
    * Adds a constraint of type t&#8712;&#10214;x&#10215;.
    */
  def addConstantConstraint(t: V, x: V): Unit = {
    log.verb(s"Adding constraint $t \u2208 \u27E6$x\u27E7")
    addToken(t, x)
    propagate()
  }

  /**
    * Adds a constraint of type &#10214;x&#10215;&#8838;&#10214;y&#10215;.
    */
  def addSubsetConstraint(x: V, y: V): Unit = {
    log.verb(s"Adding constraint \u27E6$x\u27E7 \u2286 \u27E6$y\u27E7")
    addEdge(x, y)
    propagate()
  }

  /**
    * Adds a constraint of type &#8704;<i>t</i>&#8712;&#10214;x&#10215;: &#10214;<i>t</i>&#10215;&#8838;&#10214;y&#10215;.
    */
  def addUniversallyQuantifiedFromConstraint(x: V, y: V): Unit = {
    log.verb(s"Adding constraint \u2200t \u2208 \u27E6$x\u27E7: \u27E6t\u27E7 \u2286 \u27E6$y\u27E7")
    val xn = getOrMakeNode(x)
    xn.fromTriggers += y
    for (t <- xn.tokenSol)
      addEdge(intToToken(t), y)
    propagate()
  }

  /**
    * Adds a constraint of type &#8704;<i>t</i>&#8712;&#10214;x&#10215;: &#10214;y&#10215;&#8838;&#10214;<i>t</i>&#10215;.
    */
  def addUniversallyQuantifiedToConstraint(x: V, y: V): Unit = {
    log.verb(s"Adding constraint \u2200t \u2208 \u27E6$x\u27E7: \u27E6$y\u27E7 \u2286 \u27E6t\u27E7")
    val xn = getOrMakeNode(x)
    xn.toTriggers += y
    for (t <- xn.tokenSol)
      addEdge(y, intToToken(t))
    propagate()
  }

  /**
    * Returns the current solution as a map from variables to token sets.
    */
  def getSolution: Map[V, Set[V]] =
    varToNode.keys.map(v => v -> getOrMakeNode(v).tokenSol.map(intToToken).toSet).toMap
}
