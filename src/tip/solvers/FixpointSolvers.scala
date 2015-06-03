package tip.solvers

import tip.lattices._

import scala.collection.immutable._

/**
 * Base trait for lattice solvers.
 */
trait LatticeSolver {

  /**
   * The lattice used by the solver.
   */
  val lattice: Lattice

  /**
   * The solver function.
   */
  def analyze(): lattice.Element
}

/**
 * Simple fixpoint solver.
 */
trait SimpleFixpointSolver extends LatticeSolver {

  /**
   * The update function for which the least fixpoint is to be computed.
   * @param x the input lattice element
   * @return the output lattice element
   */
  def fun(x: lattice.Element): lattice.Element

  /**
   * The basic Knaster-Tarski/Kleene fixpoint solver.
   */
  def analyze(): lattice.Element = {
    var x = lattice.bottom
    var t = x
    do {
      t = x
      x = fun(x)
    } while (x != t)
    x
  }
}

/**
 * Base trait for map lattice solvers. 
 * @tparam N type of the elements in the map domain.
 */
trait MapLatticeSolver[N] extends LatticeSolver {

  /**
   * Must be a map lattice.
   */
  val lattice: MapLattice[N, Lattice]

  /**
   * The update function for individual elements in the map domain.
   * @param n the current location in the map domain 
   * @param s the input sublattice element at the current location in the map domain
   * @param o the entire input lattice element  
   * @return the output sublattice element
   */
  def funsub(n: N, s: lattice.sublattice.Element, o: lattice.Element): lattice.sublattice.Element
}

/**
 * A general update function for map lattices.
 * @tparam N type of the elements in the map domain.
 */
trait MapLatticeUpdateFunction[N] extends MapLatticeSolver[N] {

  /**
   * The map domain.
   */
  val domain: Set[N]

  /**
   * The function for which the least fixpoint is to be computed.
   * Applies the sublattice update function pointwise to each entry.
   * @param x the input lattice element
   * @return the output lattice element
   */
  def fun(x: lattice.Element): lattice.Element = {
    domain.foldLeft(lattice.bottom)((m, a) => m + (a -> funsub(a, x(a), x)))
  }
}

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
 * A general update function for map lattices with lifted co-domains.
 * @tparam N type of the elements in the map domain.
 */
trait MapLiftLatticeUpdateFunction[N] extends Dependencies[N] {

  val lattice: MapLattice[N, LiftLattice[Lattice]]

  /**
   * The transfer function.
   */
  def transfer(n: N, s: lattice.sublattice.sublattice.Element): lattice.sublattice.sublattice.Element

  /**
   * Computes the least upper bound of the (implicitly) unlifted incoming elements.
   */
  def join(n: N, s: lattice.sublattice.Element, o: lattice.Element): lattice.sublattice.Element = {
    import lattice.sublattice._
    val states = indep(n).map(o(_))
    states.foldLeft(lattice.sublattice.sublattice.bottom)((acc, pred) => lattice.sublattice.sublattice.lub(acc, pred))
  }

  /**
   * Update function that first computes the join of the incoming elements and applies the transfer function.
   * (Implicitly) unlifts/lifts before/after applying the transfer function.
   */
  def funsub(n: N, s: lattice.sublattice.Element, o: lattice.Element): lattice.sublattice.Element = {
    import lattice.sublattice._
    transfer(n, join(n, s, o))
  }
}

/**
 * An abstract worklist algorithm.
 * @tparam N type of the elements in the worklist.
 */
trait Worklist[N] {

  /**
   * Called by [[run]] to process an item from the worklist.
   */
  def apply(n: N)

  /**
   * Adds an item to the worklist.
   */
  def add(n: N)

  /**
   * Adds a set of items to the worklist.
   */
  def add(ns: Set[N])

  /**
   * Iterates until there is no more work to do.
   * @param first the initial contents of the worklist
   */
  def run(first: Set[N])
}

/**
 * A simple worklist algorithm based on a [[scala.collection.immutable.ListSet]].
 * (Using a priority queue would typically be faster.)
 * @tparam N type of the elements in the worklist.
 */
trait ListSetWorklist[N] extends Worklist[N] {

  private var worklist = new ListSet[N]

  def add(n: N) = {
    worklist += n
  }

  def add(ns: Set[N]) = {
    worklist ++= ns
  }

  def run(first: Set[N]) = {
    worklist = new ListSet[N] ++ first
    while (worklist.nonEmpty) {
      val n = worklist.head; worklist = worklist.tail
      apply(n)
    }
  }
}

/**
 * Worklist-based fixpoint solver.
 * @tparam N type of the elements in the worklist.
 */
trait WorklistFixpointSolver[N] extends MapLatticeSolver[N] with ListSetWorklist[N] with Dependencies[N] {

  /**
   * The current lattice element.
   */
  var x = lattice.bottom

  /**
   * The map domain.
   */
  val domain: Set[N]

  def apply(n: N) = {
    val xn = x(n)
    val y = funsub(n, xn, x)
    if (y != xn) {
      x += n -> y
      add(outdep(n))
    }
  }

  def analyze(): lattice.Element = {
    run(domain)
    x
  }
}

/**
 * Worklist-based fixpoint solver that performs propagation after transfer instead of join before transfer.
 * This results in fewer join operations when nodes have many dependencies.
 * Note that with this approach, each abstract state represents the program point *after* the node
 * (for a forward analysis, and opposite for a backward analysis).
 */
trait WorklistFixpointPropagationSolver[N] extends WorklistFixpointSolver[N] {

  val lattice: MapLattice[N, LiftLattice[Lattice]]

  /**
   * The transfer function.
   */
  def transfer(n: N, s: lattice.sublattice.sublattice.Element): lattice.sublattice.sublattice.Element

  /**
   * Propagates lattice element y to node m.
   */
  def propagate(y: lattice.sublattice.Element, m: N) = {
    val xm = x(m)
    val t = lattice.sublattice.lub(xm, y)
    if (t != xm) {
      add(m);
      x += m -> t
    }
  }

  /**
   * This method overrides the one from [[WorklistFixpointSolver]].
   * Called by the worklist solver when a node is visited.
   */
  override def apply(n: N) = {
    // read the current lattice element
    val xn = x(n)
    // apply the transfer function
    import lattice.sublattice._
    val y = transfer(n, xn)
    // propagate to all nodes that depend on this one
    for (m <- outdep(n)) propagate(y, m)
  }
}

/**
 * The worklist-based fixpoint solver with initialization.
 */
trait WorklistFixpointSolverWithInit[N] extends WorklistFixpointSolver[N] {

  /**
   * Must be a map lattice, with a lifted state.
   */
  val lattice: MapLattice[N, LiftLattice[_]]

  /**
   * The start locations.
   */
  val first: Set[N]

  /**
   * The initial lattice element at the start location.
   * Default: bottom.
   */
  val init = lattice.sublattice.bottom

  override def analyze(): lattice.Element = {
    x = first.foldLeft(lattice.bottom) { (l, cur) => l + (cur -> init) }
    run(first)
    x
  }
}

/**
 * Worklist-based fixpoint solver with initialization and set widening.
 */
trait WorklistFixpointSolverWithInitAndSetWidening[N] extends WorklistFixpointSolverWithInit[N] {

  /**
   * Set widening function.
   * @param s input lattice element
   * @return output lattice element
   */
  def widen(s: lattice.sublattice.Element): lattice.sublattice.Element

  /**
   * Tells whether (src,dst) is a back-edge.
   */
  def backedge(src: N, dst: N): Boolean

  override def apply(n: N) = {
    val xn = x(n)
    val y = funsub(n, xn, x)
    if (y != xn) {
      x += n -> (if (outdep(n).exists(backedge(n, _))) widen(y) else y)
      add(outdep(n))
    }
  }
}

/**
 * Provides method for narrowing.
 */
trait MapLatticeNarrowing[N] extends MapLatticeUpdateFunction[N] {

  /**
   * Performs narrowing on the given lattice element
   * @param x the lattice element
   * @param i number of iterations
   */
  def narrow(x: lattice.Element, i: Int): lattice.Element = {
    if (i <= 0) x else narrow(fun(x), i - 1)
  }
}

/**
 * The worklist-based fixpoint solver with initialization, set widening, and narrowing.
 */
trait WorklistFixpointSolverWithInitAndSetWideningAndNarrowing[N] extends WorklistFixpointSolverWithInitAndSetWidening[N]
with MapLatticeNarrowing[N] {

  /**
   * Number of narrowing steps.
   */
  val narrowingSteps: Int

  override def analyze(): lattice.Element = {
    narrow(super.analyze(), narrowingSteps)
  }
}
