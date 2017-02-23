package tip.solvers

import tip.util.Log

import scala.collection.mutable
import scala.language.implicitConversions

/**
  * The cubic solver.
  * @tparam V type of variables
  * @tparam T type of tokens
  */
class CubicSolver[V, T]() {

  val log = Log.logger[this.type]()

  var lastTknId = -1

  def nextTokenId = {
    lastTknId += 1; lastTknId
  }

  class Node(val succ: mutable.Set[V] = mutable.Set(), // note: the edges between nodes go via the variables
             val tokenSol: mutable.BitSet = new mutable.BitSet(), // the current solution bitvector
             val conditionals: mutable.Map[Int, mutable.Set[(V, V)]] = mutable.Map() // the pending conditional constraints
  ) {
    override def toString = this.hashCode().toString
  }

  /**
    * The map from variables to nodes.
    */
  val nodeState: mutable.Map[V, Node] = mutable.Map()

  /**
    * Provides an index for each token that we have seen.
    */
  val tokenToInt: mutable.Map[T, Int] = mutable.Map()

  /**
    * Returns the index associated with the given token.
    * Allocates a fresh index if the token hasn't been seen before.
    */
  implicit private def getTokenInt(tkn: T): Int = {
    tokenToInt.getOrElseUpdate(tkn, nextTokenId)
  }

  /**
    * Retrieves the node associated with the given variable.
    * Allocates a fresh node if the variable hasn't been seen before.
    */
  private def getOrPutNode(x: V): Node = {
    nodeState.getOrElseUpdate(x, new Node())
  }

  /**
    * Detects a cycle along the graph.
    */
  private def detectCycles(first: V, current: V, visited: Set[V]): Set[V] = {
    val currentNode = getOrPutNode(current)
    val firstNode = getOrPutNode(first)
    if (currentNode != firstNode && visited.contains(current)) {
      Set()
    } else if (currentNode == firstNode && visited.contains(current)) {
      visited
    } else {
      val cycles = currentNode.succ.toSet.map { v: V =>
        detectCycles(first, v, visited + current)
      }
      cycles.flatten
    }
  }

  /**
    * Collapses the given cycle (if nonempty).
    */
  private def collapseCycle(cycle: Set[V]) {
    if (cycle.nonEmpty) {
      log.verb(s"Collapsing cycle $cycle")
      val first = getOrPutNode(cycle.head)
      cycle.tail.foreach { cso =>
        val oldState = getOrPutNode(cso)
        first.succ ++= oldState.succ
        first.conditionals.keys.foreach { k =>
          first.conditionals(k) ++= oldState.conditionals(k)
        }
        first.tokenSol |= oldState.tokenSol
        nodeState(cso) = first
      }
    }
  }

  /**
    * Adds the set of tokens `s` to the variable `x` and propagates along the graph.
    */
  private def addAndPropagateBits(s: mutable.BitSet, x: V) {
    val state = getOrPutNode(x)
    val old = state.tokenSol.clone()
    val newTokens = old | s
    if (newTokens != old) {
      // Set the new bits
      state.tokenSol |= s
      val diff = newTokens &~ old

      // Add edges from pending lists, then clear the lists
      diff.foreach { t =>
        state.conditionals.getOrElse(t, Set()).foreach {
          case (v1, v2) =>
            addSubsetConstraint(v1, v2)
        }
      }
      diff.foreach { t =>
        state.conditionals.remove(t)
      }

      // Propagate to successors
      state.succ.foreach { s =>
        addAndPropagateBits(newTokens, s)
      }
    }
  }

  /**
    * Adds a constraint of type <i>t</i> &#8712; <i>x</i>.
    */
  def addConstantConstraint(t: T, x: V): Unit = {
    log.verb(s"Adding constraint $t \u2208 [[$x]]")
    val bs = new mutable.BitSet()
    bs.add(t)
    addAndPropagateBits(bs, x)
  }

  /**
    * Adds a constraint of type <i>x</i> &#8838; <i>y</i>.
    */
  def addSubsetConstraint(x: V, y: V): Unit = {
    log.verb(s"Adding constraint [[$x]] \u2286 [[$y]]")
    val nx = getOrPutNode(x)
    getOrPutNode(y)

    // Add the edge
    log.verb(s"Adding edge $x -> $y")
    nx.succ += y

    // Collapse newly introduced cycle
    collapseCycle(detectCycles(x, x, Set()))

    // Propagate the bits
    addAndPropagateBits(nx.tokenSol, y)
  }

  /**
    * Adds a constraint of type <i>t</i> &#8712; <i>x</i> &#8658; <i>y</i> &#8838; <i>z</i>.
    */
  def addConditionalConstraint(t: T, x: V, y: V, z: V): Unit = {
    log.verb(s"Adding constraint $t \u2208 [[$x]] => [[$y]] \u2286 [[$z]]")
    val xn = getOrPutNode(x)
    if (xn.tokenSol.contains(t)) {
      // Already enabled
      addSubsetConstraint(y, z)
    } else {
      // Not yet enabled, add to pending list
      log.verb(s"Condition $t \u2208 [[$x]] not yet enabled, adding ([[$y]],[[$z]]) to pending")
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
    nodeState.keys.map(v => v -> getOrPutNode(v).tokenSol.map(i => intToToken(i)).toSet).toMap
  }
}
