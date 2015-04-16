package tip.solvers

import tip.lattices._
import scala.collection.immutable._

/**
 * The "naive" fixpoint solver.
 */
trait SimpleFixpointSolver {
  
  val lattice: Lattice

  def transfer(x: lattice.Element): lattice.Element

  def analyze(): lattice.Element = {
    var x = lattice.bottom
    var t = x
    do {
      t = x
      x = transfer(x)
    } while (x != t)
    x
  }
}

/**
 * A general transfer function for map lattices.
 */
trait MapNodeTransfer[N] {

  val lattice: MapLattice[N, Lattice]
  
  val domain: Set[N]
  
  def transfer(n: N, s: lattice.l.Element, o: lattice.Element): lattice.l.Element

  def transfer(x: lattice.Element): lattice.Element = {
    domain.foldLeft(lattice.bottom)((m, a) => lattice.set(m, a, transfer(a, lattice.getsome(x, a), x)))
  }
}

/**
 * The worklist-based fixpoint solver.
 * This version uses a listset for the worklist (using a priority queue would typically be faster).
 * Also, it uses all nodes in the initial worklist (using only the extremal node would typically be faster).
 */
trait WorklistFixpointSolver[N] {
  
  val lattice: MapLattice[N, Lattice]
  
  val domain: Set[N]
  
  def dep(n: N): Set[N]

  def transfer(n: N, s: lattice.l.Element, o: lattice.Element): lattice.l.Element

  def analyze(): lattice.Element = {
    var x = lattice.bottom
    var w = new ListSet[N] ++ domain
    while (!w.isEmpty) {
      val n = w.head; w = w.tail
      val xn = lattice.getsome(x, n)
      val y = transfer(n, xn, x)
      if (y != xn) {
        w = w ++ dep(n)
        x = lattice.set(x, n, y)
      }
    }
    x
  }
}
