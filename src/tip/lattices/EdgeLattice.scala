package tip.lattices

/**
  * The lattice of edge functions, used by [[tip.solvers.IDEAnalysis]].
  * Technically a map lattice, but we don't bother implementing it as extension of MapLattice.
  */
class EdgeLattice[L <: Lattice](val valuelattice: L) extends Lattice {

  type Element = Edge

  val bottom = ConstEdge(valuelattice.bottom)

  def lub(x: Element, y: Element): Element = x.joinWith(y)

  /**
    * An "edge" represents a function L -> L where L is the value lattice.
    */
  trait Edge extends (valuelattice.Element => valuelattice.Element) {

    /**
      * Applies the function to the given lattice element.
      */
    def apply(x: valuelattice.Element): valuelattice.Element

    /**
      * Composes this function with the given one.
      * The resulting function first applies `e` then this function.
      */
    def composeWith(e: Edge): Edge

    /**
      * Finds the least upper bound of this function and the given one.
      */
    def joinWith(e: Edge): Edge
  }

  /**
    * Edge labeled with identity function.
    */
  case class IdEdge() extends Edge {

    def apply(x: valuelattice.Element): valuelattice.Element = x

    def composeWith(e: Edge): Edge = e

    def joinWith(e: Edge): Edge =
      if (e == this) this
      else e.joinWith(this)

    override def toString = "IdEdge()"
  }

  /**
    * Edge labeled with constant function.
    */
  case class ConstEdge(c: valuelattice.Element) extends Edge {

    def apply(x: valuelattice.Element): valuelattice.Element = c

    def composeWith(e: Edge): Edge = this

    def joinWith(e: Edge): Edge =
      if (e == this || c == valuelattice.top) this
      else if (c == valuelattice.bottom) e
      else
        e match {
          case IdEdge() => ??? // never reached with the currently implemented analyses
          case ConstEdge(ec) => ConstEdge(valuelattice.lub(c, ec))
          case _ => e.joinWith(this)
        }

    override def toString = s"ConstEdge($c)"
  }
}
