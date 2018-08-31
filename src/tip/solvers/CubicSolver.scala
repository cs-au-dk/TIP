package tip.solvers

import tip.util.Log

import scala.collection.mutable
import scala.language.implicitConversions

/**
  * The cubic solver.
  *
  * @param cycleElimination: whether to use cycle elimination or not
  * @tparam V type of variables
  * @tparam T type of tokens
  */
class CubicSolver[V, T](cycleElimination: Boolean = true) {

  val log = Log.logger[this.type]()

  var lastTknId: Int = -1

  def nextTokenId: Int = {
    lastTknId += 1; lastTknId
  }

  class Node(
    val succ: mutable.Set[V] = mutable.Set(), // note: the edges between nodes go via the variables
    val tokenSol: mutable.BitSet = new mutable.BitSet(), // the current solution bitvector
    val conditionals: mutable.Map[Int, mutable.Set[(V, V)]] = mutable.Map(), // the pending conditional constraints
    val vars: mutable.Set[V] = mutable.Set() // the variables belonging to this node
  ) {
    def this(x: V) {
      this()
      vars += x
    }

    override def toString = this.hashCode.toString
  }

  /**
    * The map from variables to nodes.
    */
  val varToNode: mutable.Map[V, Node] = mutable.Map()

  /**
    * Provides an index for each token that we have seen.
    */
  val tokenToInt: mutable.Map[T, Int] = mutable.Map()

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
  private def getOrPutNode(x: V): Node =
    varToNode.getOrElseUpdate(x, new Node(x))

  /**
    * Attempts to detect a path from `from` to `to` in the graph.
    * @return the list of variables in the path if such path is found, an empty list otherwise.
    */
  private def detectPath(from: Node, to: Node): List[Node] = {
    val visited: mutable.Set[Node] = mutable.Set()

    def detectPathRec(current: Node): List[Node] =
      if (current == to) {
        // Detected a path from from to to
        List(current)
      } else {
        visited += current
        // Search for the first cycle we can find, and save it
        // If no cycle is found, return the empty list
        var toReturn: List[Node] = List()
        current.succ
          .map(varToNode(_))
          .toSet
          .filter(!visited.contains(_))
          .exists { n: Node =>
            val cycleVisited = detectPathRec(n)
            if (cycleVisited.nonEmpty) {
              // Cycle found
              toReturn = current :: cycleVisited
              true
            } else false // keep searching
          }
        toReturn
      }
    val res = detectPathRec(from)
    res
  }

  /**
    * Collapses the given cycle (if nonempty).
    */
  private def collapseCycle(cycle: List[Node]) {
    if (cycle.nonEmpty) {
      log.verb(s"Collapsing cycle $cycle")
      val first = cycle.head
      cycle.tail.foreach { oldNode =>
        // Merge oldNode into first
        first.succ ++= oldNode.succ
        first.conditionals.keys.foreach { k =>
          first.conditionals(k) ++= oldNode.conditionals(k)
        }
        first.tokenSol |= oldNode.tokenSol
        // Redirect all the variables that were pointing to this node to the new one
        oldNode.vars.foreach { v =>
          varToNode(v) = first
          first.vars += v
        }
      }
    }
  }

  /**
    * Adds the set of tokens `s` to the variable `x` and propagates along the graph.
    */
  private def addAndPropagateBits(s: mutable.BitSet, x: V) {
    val node = getOrPutNode(x)
    val old = node.tokenSol.clone()
    val newTokens = old | s
    if (newTokens != old) {
      // Set the new bits
      node.tokenSol |= s
      val diff = newTokens &~ old

      // Add edges from pending lists, then clear the lists
      diff.foreach { t =>
        node.conditionals.getOrElse(t, Set()).foreach {
          case (v1, v2) =>
            addSubsetConstraint(v1, v2)
        }
      }
      diff.foreach { t =>
        node.conditionals.remove(t)
      }

      // Propagate to successors
      node.succ.foreach { s =>
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
    val ny = getOrPutNode(y)

    if (nx != ny) {
      // Add the edge
      log.verb(s"Adding edge $x -> $y")
      nx.succ += y

      // Propagate the bits
      addAndPropagateBits(nx.tokenSol, y)

      // Collapse newly introduced cycle, if any
      if (cycleElimination)
        collapseCycle(detectPath(ny, nx))
    }
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
    } else if (y != z) {
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
    varToNode.keys.map(v => v -> getOrPutNode(v).tokenSol.map(i => intToToken(i)).toSet).toMap
  }
}
