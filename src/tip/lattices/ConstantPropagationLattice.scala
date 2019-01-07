package tip.lattices

/**
  * Constant propagation lattice.
  */
object ConstantPropagationLattice extends FlatLattice[Int]() with LatticeWithOps {

  private def apply(op: (Int, Int) => Int, a: Element, b: Element): Element = (a, b) match {
    case (FlatEl(x), FlatEl(y)) => FlatEl(op(x, y))
    case (Bot, _) => Bot
    case (_, Bot) => Bot
    case (_, Top) => Top
    case (Top, _) => Top
  }

  def num(i: Int): Element = FlatEl(i)

  def plus(a: Element, b: Element): Element = apply(_ + _, a, b)

  def minus(a: Element, b: Element): Element = apply(_ - _, a, b)

  def times(a: Element, b: Element): Element = apply(_ * _, a, b)

  def div(a: Element, b: Element): Element = apply((x, y) => if (y != 0) x / y else Bot, a, b)

  def eqq(a: Element, b: Element): Element = apply((x, y) => if (x == y) 1 else 0, a, b)

  def gt(a: Element, b: Element): Element = apply((x, y) => if (x > y) 1 else 0, a, b)
}
