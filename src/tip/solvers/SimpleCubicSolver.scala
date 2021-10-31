package tip.solvers

import tip.util.Log

import scala.collection.mutable
import scala.language.implicitConversions

/**
  * Simple cubic solver.
  *
  * @tparam V type of variables
  * @tparam T type of tokens
  */
class SimpleCubicSolver[V, T] {

  private val log = Log.logger[this.type]()

  private var lastTknId: Int = -1

  private def nextTokenId: Int = {
    lastTknId += 1; lastTknId
  }

  private class Node(
    val succ: mutable.Set[V] = mutable.Set(), // note: the edges between nodes go via the variables
    val tokenSol: mutable.BitSet = new mutable.BitSet(), // the current solution bitvector
    val conditionals: mutable.Map[Int, mutable.Set[(V, V)]] = mutable.Map() // the pending conditional constraints
  )

  /**
    * The map from variables to nodes.
    */
  private val varToNode: mutable.Map[V, Node] = mutable.Map()

  /**
    * Provides an index for each token that we have seen.
    */
  private val tokenToInt: mutable.Map[T, Int] = mutable.Map()

  /**
    * Worklist of (token, variable) pairs.
    */
  private val worklist: mutable.Queue[(Int, V)] = mutable.Queue()

  /**
    * Returns the index associated with the given token.
    * Allocates a fresh index if the token hasn't been seen before.
    */
  implicit private def getTokenInt(tkn: T): Int =
    tokenToInt.getOrElseUpdate(tkn, nextTokenId)

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
      // Process pending constraints
      val nx = getOrMakeNode(x)
      nx.conditionals.remove(t).foreach { s =>
        for ((y, z) <- s)
          addEdge(y, z)
      }
      // Propagate token to successors
      for (v <- nx.succ)
        addToken(t, v)
    }

  /**
    * Adds a constraint of type t&#8712;&#10214;x&#10215;.
    */
  def addConstantConstraint(t: T, x: V): Unit = {
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
    * Adds a constraint of type t&#8712;&#10214;x&#10215;&#8658;&#10214;y&#10215;&#8838;&#10214;z&#10215;.
    */
  def addConditionalConstraint(t: T, x: V, y: V, z: V): Unit = {
    log.verb(s"Adding constraint $t \u2208 \u27E6$x\u27E7 \u21D2 \u27E6$y\u27E7 \u2286 \u27E6$z\u27E7")
    val xn = getOrMakeNode(x)
    if (xn.tokenSol.contains(t)) {
      // Already enabled
      addSubsetConstraint(y, z)
    } else if (y != z) {
      // Not yet enabled, add to pending list
      log.verb(s"Condition $t \u2208 \u27E6$x\u27E7 not yet enabled, adding (\u27E6$y\u27E7,\u27E6$z\u27E7) to pending")
      xn.conditionals
        .getOrElseUpdate(t, mutable.Set[(V, V)]())
        .add((y, z))
    }
  }

  /**
    * Returns the current solution as a map from variables to token sets.
    */
  def getSolution: Map[V, Set[T]] = {
    val intToToken = tokenToInt.map(p => p._2 -> p._1).toMap[Int, T]
    varToNode.keys.map(v => v -> getOrMakeNode(v).tokenSol.map(intToToken).toSet).toMap
  }
}
