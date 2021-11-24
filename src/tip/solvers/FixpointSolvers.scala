package tip.solvers

import tip.analysis.Dependencies
import tip.lattices._
import tip.util.Log

import scala.collection.immutable._

object FixpointSolvers {

  val log = Log.logger[this.type]()
}

/**
  * Base trait for lattice solvers.
  */
trait LatticeSolver {

  /**
    * The lattice used by the solver.
    */
  val lattice: Lattice

  /**
    * The analyze function.
    */
  def analyze(): lattice.Element
}

/**
  * Simple fixpoint solver.
  */
trait SimpleFixpointSolver extends LatticeSolver {

  /**
    * The constraint function for which the least fixpoint is to be computed.
    * @param x the input lattice element
    * @return the output lattice element
    */
  def fun(x: lattice.Element): lattice.Element

  /**
    * The basic Kleene fixpoint solver.
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
trait MapLatticeSolver[N] extends LatticeSolver with Dependencies[N] {

  /**
    * Must be a map lattice.
    */
  val lattice: MapLattice[N, Lattice]

  /**
    * The transfer function.
    */
  def transfer(n: N, s: lattice.sublattice.Element): lattice.sublattice.Element

  /**
    * The constraint function for individual elements in the map domain.
    * First computes the join of the incoming elements and then applies the transfer function.
    * @param n the current location in the map domain
    * @param x the current lattice element for all locations
    * @return the output sublattice element
    */
  def funsub(n: N, x: lattice.Element): lattice.sublattice.Element =
    transfer(n, join(n, x))

  /**
    * Computes the least upper bound of the incoming elements.
    */
  def join(n: N, o: lattice.Element): lattice.sublattice.Element = {
    val states = indep(n).map(o(_))
    states.foldLeft(lattice.sublattice.bottom)((acc, pred) => lattice.sublattice.lub(acc, pred))
  }
}

/**
  * Simple fixpoint solver for map lattices where the constraint function is defined pointwise.
  * @tparam N type of the elements in the map domain.
  */
trait SimpleMapLatticeFixpointSolver[N] extends SimpleFixpointSolver with MapLatticeSolver[N] {

  /**
    * The map domain.
    */
  val domain: Set[N]

  /**
    * The function for which the least fixpoint is to be computed.
    * Applies the sublattice constraint function pointwise to each entry.
    * @param x the input lattice element
    * @return the output lattice element
    */
  def fun(x: lattice.Element): lattice.Element = {
    FixpointSolvers.log.verb(s"In state $x")
    domain.foldLeft(lattice.bottom)(
      (m, a) =>
        m + (a -> {
          FixpointSolvers.log.verb(s"Processing $a")
          funsub(a, x)
        })
    )
  }
}

/**
  * Base trait for solvers for map lattices with lifted co-domains.
  * @tparam N type of the elements in the map domain.
  */
trait MapLiftLatticeSolver[N] extends MapLatticeSolver[N] with Dependencies[N] {

  val lattice: MapLattice[N, LiftLattice[Lattice]]

  /**
    * The transfer function for the sub-sub-lattice.
    */
  def transferUnlifted(n: N, s: lattice.sublattice.sublattice.Element): lattice.sublattice.sublattice.Element

  def transfer(n: N, s: lattice.sublattice.Element): lattice.sublattice.Element = {
    import lattice.sublattice._
    s match {
      case Bottom => Bottom // unreachable as input implied unreachable at output
      case Lift(a) => lift(transferUnlifted(n, a))
    }
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
  def process(n: N): Unit

  /**
    * Adds an item to the worklist.
    */
  def add(n: N): Unit

  /**
    * Adds a set of items to the worklist.
    */
  def add(ns: Set[N]): Unit

  /**
    * Iterates until there is no more work to do.
    * @param first the initial contents of the worklist
    */
  def run(first: Set[N]): Unit
}

/**
  * A simple worklist algorithm based on `scala.collection.immutable.ListSet`.
  * (Using a priority queue would typically be faster.)
  * @tparam N type of the elements in the worklist.
  */
trait ListSetWorklist[N] extends Worklist[N] {

  private var worklist = new ListSet[N]

  def add(n: N) = {
    FixpointSolvers.log.verb(s"Adding $n to worklist")
    worklist += n
  }

  def add(ns: Set[N]) = {
    FixpointSolvers.log.verb(s"Adding $ns to worklist")
    worklist ++= ns
  }

  def run(first: Set[N]) = {
    worklist = new ListSet[N] ++ first
    while (worklist.nonEmpty) {
      val n = worklist.head; worklist = worklist.tail
      process(n)
    }
  }
}

/**
  * Base trait for worklist-based fixpoint solvers.
  * @tparam N type of the elements in the worklist.
  */
trait WorklistFixpointSolver[N] extends MapLatticeSolver[N] with ListSetWorklist[N] with Dependencies[N] {

  /**
    * The current lattice element.
    */
  var x: lattice.Element = _

  def process(n: N) = {
    val xn = x(n)
    FixpointSolvers.log.verb(s"Processing $n, current state: $xn")
    val y = funsub(n, x)
    FixpointSolvers.log.verb(s"New state: $y")
    if (y != xn) {
      x += n -> y
      add(outdep(n))
    }
  }
}

/**
  * Worklist-based fixpoint solver.
  * @tparam N type of the elements in the worklist.
  */
trait SimpleWorklistFixpointSolver[N] extends WorklistFixpointSolver[N] {

  /**
    * The map domain.
    */
  val domain: Set[N]

  def analyze(): lattice.Element = {
    x = lattice.bottom
    run(domain)
    x
  }
}

/**
  * The worklist-based fixpoint solver with reachability.
  *
  * This solver works for map lattices with lifted co-domains, where the extra bottom element typically represents "unreachable".
  */
trait WorklistFixpointSolverWithReachability[N] extends WorklistFixpointSolver[N] with MapLiftLatticeSolver[N] {

  /**
    * The start locations, used as the initial contents of the worklist.
    */
  val first: Set[N]

  def analyze(): lattice.Element = {
    x = lattice.bottom
    run(first)
    x
  }
}

/**
  * Functions for solvers that perform propagation after transfer instead of join before transfer.
  */
trait WorklistFixpointPropagationFunctions[N] extends ListSetWorklist[N] {

  /**
    * Must be a map lattice.
    */
  val lattice: MapLattice[N, Lattice]

  /**
    * The current lattice element.
    */
  var x: lattice.Element

  /**
    * The start locations.
    */
  val first: Set[N]

  /**
    * The initial lattice element at the start locations.
    */
  def init: lattice.sublattice.Element

  /**
    * Propagates lattice element y to node m.
    */
  def propagate(y: lattice.sublattice.Element, m: N) = {
    FixpointSolvers.log.verb(s"Propagating $y to $m")
    val xm = x(m)
    val t = lattice.sublattice.lub(xm, y)
    if (t != xm) {
      add(m)
      x += m -> t
    }
  }

  def analyze(): lattice.Element = {
    x = first.foldLeft(lattice.bottom) { (l, cur) =>
      l + (cur -> init)
    }
    run(first)
    x
  }
}

/**
  * Worklist-based fixpoint solver that performs propagation after transfer instead of join before transfer.
  * This results in fewer join operations when nodes have many dependencies.
  * Note that with this approach, each abstract state represents the program point *before* the node
  * (for a forward analysis, and opposite for a backward analysis).
  */
trait WorklistFixpointPropagationSolver[N] extends WorklistFixpointSolverWithReachability[N] with WorklistFixpointPropagationFunctions[N] {

  val lattice: MapLattice[N, LiftLattice[Lattice]]

  /**
    * The initial lattice element at the start locations.
    * Default: lift(bottom).
    */
  def init: lattice.sublattice.Element = lattice.sublattice.lift(lattice.sublattice.sublattice.bottom)

  /**
    * This method overrides the one from [[WorklistFixpointSolver]].
    * Called by the worklist solver when a node is visited.
    */
  override def process(n: N): Unit = {
    // read the current lattice element
    val xn = x(n)
    // apply the transfer function
    FixpointSolvers.log.verb(s"Processing $n in state $xn")
    val y = transfer(n, xn)
    FixpointSolvers.log.verb(s"Resulting state: $y")
    // propagate to all nodes that depend on this one
    for (m <- outdep(n)) propagate(y, m)
  }

  override def analyze(): lattice.Element = {
    x = first.foldLeft(lattice.bottom) { (l, cur) =>
      l + (cur -> init)
    }
    run(first)
    x
  }
}

/**
  * Worklist-based fixpoint solver with reachability and widening.
  */
trait WorklistFixpointSolverWithReachabilityAndWidening[N] extends WorklistFixpointSolverWithReachability[N] {

  /**
    * Widening function.
    * @param x lattice element from previous iteration
    * @param y lattice element from this iteration
    * @return output lattice element
    */
  def widen(x: lattice.sublattice.Element, y: lattice.sublattice.Element): lattice.sublattice.Element

  /**
    * Tells whether `n` is a loop-head.
    */
  def loophead(n: N): Boolean

  override def process(n: N) = {
    val xn = x(n)
    FixpointSolvers.log.verb(s"Processing $n, current state $xn")
    val y = funsub(n, x)
    FixpointSolvers.log.verb(s"New state: $y")
    if (y != xn) {
      x += n -> (if (loophead(n)) widen(xn, y) else y)
      add(outdep(n))
    }
  }
}

/**
  * The worklist-based fixpoint solver with reachability, widening, and (simple) narrowing.
  */
trait WorklistFixpointSolverWithReachabilityAndWideningAndNarrowing[N]
    extends WorklistFixpointSolverWithReachabilityAndWidening[N]
    with SimpleMapLatticeFixpointSolver[N] {

  /**
    * Number of narrowing steps.
    */
  val narrowingSteps: Int

  /**
    * Performs narrowing on the given lattice element
    * @param x the lattice element
    * @param i number of iterations
    */
  def narrow(x: lattice.Element, i: Int): lattice.Element =
    if (i <= 0) x else narrow(fun(x), i - 1) // uses the simple definition of 'fun' from SimpleMapLatticeFixpointSolver

  override def analyze(): lattice.Element =
    narrow(super[WorklistFixpointSolverWithReachabilityAndWidening].analyze(), narrowingSteps)
}
