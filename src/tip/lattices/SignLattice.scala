package tip.lattices

/**
  * An element of the sign lattice.
  */
object SignElement extends Enumeration {
  val Pos, Neg, Zero = Value
}

/**
  * The sign lattice.
  */
object SignLattice extends FlatLattice[SignElement.Value] with LatticeWithOps {

  import SignElement._

  private val signValues: Map[Element, Int] = Map(Bot -> 0, FlatEl(Zero) -> 1, FlatEl(Neg) -> 2, FlatEl(Pos) -> 3, Top -> 4)

  private def lookup(op: List[List[Element]], x: Element, y: Element): Element =
    op(signValues(x))(signValues(y))

  private val absPlus: List[List[Element]] =
    List(
      List(Bot, Bot, Bot, Bot, Bot),
      List(Bot, Zero, Neg, Pos, Top),
      List(Bot, Neg, Neg, Top, Top),
      List(Bot, Pos, Top, Pos, Top),
      List(Bot, Top, Top, Top, Top)
    )

  private val absMinus: List[List[Element]] =
    List(
      List(Bot, Bot, Bot, Bot, Bot),
      List(Bot, Zero, Pos, Neg, Top),
      List(Bot, Neg, Top, Neg, Top),
      List(Bot, Pos, Pos, Top, Top),
      List(Bot, Top, Top, Top, Top)
    )

  private val absTimes: List[List[Element]] =
    List(
      List(Bot, Bot, Bot, Bot, Bot),
      List(Bot, Zero, Zero, Zero, Zero),
      List(Bot, Zero, Pos, Neg, Top),
      List(Bot, Zero, Neg, Pos, Top),
      List(Bot, Zero, Top, Top, Top)
    )

  private val absDivide: List[List[Element]] =
    List(
      List(Bot, Bot, Bot, Bot, Bot),
      List(Bot, Bot, Zero, Zero, Top),
      List(Bot, Bot, Top, Top, Top),
      List(Bot, Bot, Top, Top, Top),
      List(Bot, Bot, Top, Top, Top)
    )

  private val absGt: List[List[Element]] =
    List(
      List(Bot, Bot, Bot, Bot, Bot),
      List(Bot, Zero, Pos, Zero, Top),
      List(Bot, Zero, Top, Zero, Top),
      List(Bot, Pos, Pos, Top, Top),
      List(Bot, Top, Top, Top, Top)
    )

  private val absEq: List[List[Element]] =
    List(
      List(Bot, Bot, Bot, Bot, Bot),
      List(Bot, Pos, Zero, Zero, Top),
      List(Bot, Zero, Top, Zero, Top),
      List(Bot, Zero, Zero, Top, Top),
      List(Bot, Top, Top, Top, Top)
    )

  def num(i: Int): Element =
    if (i == 0)
      Zero
    else if (i > 0)
      Pos
    else
      Neg

  def plus(a: Element, b: Element): Element = lookup(absPlus, a, b)

  def minus(a: Element, b: Element): Element = lookup(absMinus, a, b)

  def times(a: Element, b: Element): Element = lookup(absTimes, a, b)

  def div(a: Element, b: Element): Element = lookup(absDivide, a, b)

  def eqq(a: Element, b: Element): Element = lookup(absEq, a, b)

  def gt(a: Element, b: Element): Element = lookup(absGt, a, b)
}
