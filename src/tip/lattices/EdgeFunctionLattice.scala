package tip.lattices

/**
  * The lattice of edge functions, used by [[tip.solvers.IDEAnalysis]].
  * A map lattice, but maps are represent differently than in `MapLattice`.
  * Currently only supports the identity function and constant functions.
  */
class EdgeFunctionLattice[L <: Lattice](val valuelattice: L) extends Lattice {

  type Element = EdgeFunction

  val bottom = ConstEdge(valuelattice.bottom)

  def lub(x: Element, y: Element): Element = x.joinWith(y)

  /**
    * An "edge" represents a function L -> L where L is the value lattice.
    */
  trait EdgeFunction extends (valuelattice.Element => valuelattice.Element) {

    /**
      * Applies the function to the given lattice element.
      */
    def apply(x: valuelattice.Element): valuelattice.Element

    /**
      * Composes this function with the given one.
      * The resulting function first applies `e` then this function.
      */
    def composeWith(e: EdgeFunction): EdgeFunction

    /**
      * Finds the least upper bound of this function and the given one.
      */
    def joinWith(e: EdgeFunction): EdgeFunction
  }

  /**
    * Edge labeled with identity function.
    */
  case class IdEdge() extends EdgeFunction {

    def apply(x: valuelattice.Element): valuelattice.Element = x

    def composeWith(e: EdgeFunction): EdgeFunction = e

    def joinWith(e: EdgeFunction): EdgeFunction =
      if (e == this) this
      else e.joinWith(this)

    override def toString = "IdEdge()"
  }

  /**
    * Edge labeled with constant function.
    */
  case class ConstEdge(c: valuelattice.Element) extends EdgeFunction {

    def apply(x: valuelattice.Element): valuelattice.Element = c

    def composeWith(e: EdgeFunction): EdgeFunction = this

    def joinWith(e: EdgeFunction): EdgeFunction =
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
