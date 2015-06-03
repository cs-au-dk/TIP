package tip.lattices

import tip.ast._

import scala.language.implicitConversions

/**
 * The sign lattice.
 */
object SignLattice extends Lattice with LatticeOps {
  // TODO: define via FlatLattice?

  type Element = SignElement.Value

  /**
   * An element of the sign lattice.
   */
  object SignElement extends Enumeration {
    val Bottom, Top, Pos, Neg, Zero = Value
  }

  import SignElement._

  override def bottom: Element = Bottom

  override def lub(x: Element, y: Element) = {
    if (x == Bottom || y == Top || x == y)
      y
    else if (y == Bottom || x == Top)
      x
    else
      Top
  }

  private val signValues = Map(Bottom -> 0, Zero -> 1, Neg -> 2, Pos -> 3, Top -> 4)

  private def abs(op: List[List[SignLattice.Element]], x: SignLattice.Element, y: SignLattice.Element): SignLattice.Element = {
    op(signValues(x))(signValues(y))
  }

  private val absPlus = List(
    List(Bottom, Bottom, Bottom, Bottom, Bottom),
    List(Bottom, Zero, Neg, Pos, Top),
    List(Bottom, Neg, Neg, Top, Top),
    List(Bottom, Pos, Top, Pos, Top),
    List(Bottom, Top, Top, Top, Top))

  private val absMinus = List(
    List(Bottom, Bottom, Bottom, Bottom, Bottom),
    List(Bottom, Zero, Pos, Neg, Top),
    List(Bottom, Neg, Top, Neg, Top),
    List(Bottom, Pos, Pos, Top, Top),
    List(Bottom, Top, Top, Top, Top))

  private val absTimes = List(
    List(Bottom, Bottom, Bottom, Bottom, Bottom),
    List(Bottom, Zero, Zero, Zero, Zero),
    List(Bottom, Zero, Pos, Neg, Top),
    List(Bottom, Zero, Neg, Pos, Top),
    List(Bottom, Zero, Top, Top, Top))

  private val absDivide = List(
    List(Bottom, Bottom, Bottom, Bottom, Bottom),
    List(Bottom, Top, Zero, Zero, Top),
    List(Bottom, Top, Top, Top, Top),
    List(Bottom, Top, Top, Top, Top),
    List(Bottom, Top, Top, Top, Top))

  private val absGT = List(
    List(Bottom, Bottom, Bottom, Bottom, Bottom),
    List(Bottom, Zero, Pos, Zero, Top),
    List(Bottom, Zero, Top, Zero, Top),
    List(Bottom, Pos, Pos, Top, Top),
    List(Bottom, Top, Top, Top, Top))

  private val absEq = List(
    List(Bottom, Bottom, Bottom, Bottom, Bottom),
    List(Bottom, Pos, Zero, Zero, Top),
    List(Bottom, Zero, Top, Zero, Top),
    List(Bottom, Zero, Zero, Top, Top),
    List(Bottom, Top, Top, Top, Top))

  override def sum(a: SignLattice.Element, b: SignLattice.Element) = abs(absPlus, a, b)

  override def sub(a: SignLattice.Element, b: SignLattice.Element) = abs(absMinus, a, b)

  override def prod(a: SignLattice.Element, b: SignLattice.Element) = abs(absTimes, a, b)

  override def div(a: SignLattice.Element, b: SignLattice.Element) = abs(absDivide, a, b)

  override def eqq(a: SignLattice.Element, b: SignLattice.Element) = abs(absEq, a, b)

  override def gt(a: SignLattice.Element, b: SignLattice.Element) = abs(absGT, a, b)

  /**
   * Returns the sign of i.
   */
  private def sign(i: Int): Element = {
    if (i == 0)
      Zero
    else if (i > 0)
      Pos
    else
      Neg
  }

  /**
   * Evaluate the expression exp in the abstract domain of signs,
   * assuming env is the current environment.
   */
  def eval[A](exp: AExpr, env: Map[AIdentifier, Element]): Element = {
    exp match {
      case id: AIdentifier =>
        id.meta.definition match {
          case Some(x: AIdentifier) => env(x)
          case _ => SignLattice.SignElement.Top
        }
      case intc: ANumber => sign(intc.value)
      case bin: ABinaryOp =>
        bin.operator match {
          case _: Plus =>
            sum(eval(bin.left, env), eval(bin.right, env))
          case _: Minus =>
            sub(eval(bin.left, env), eval(bin.right, env))
          case _: Times =>
            prod(eval(bin.left, env), eval(bin.right, env))
          case _: Divide =>
            div(eval(bin.left, env), eval(bin.right, env))
          case _: GreatThan =>
            gt(eval(bin.left, env), eval(bin.right, env))
          case _: Eqq =>
            eqq(eval(bin.left, env), eval(bin.right, env))
          case _ => ???
        }
      case _ => ???
    }
  }
}

/**
 * The interval lattice
 */
class IntervalLattice extends Lattice with LatticeOps {

  /**
   * The element of the IntervalLattice.
   * An interval of the form (x, y) ranges from x to y (x < y), x and y included
   * The interval (PInf, MInf) is the empty interval (the bottom)
   * The interval (MInf, PInf) is the top of the lattice
   */
  type Element = (Num, Num)

  val fullInterval = (MInf, PInf)

  val emptyInterval = (PInf, MInf)

  implicit def int2num(i: Int): IntNum = IntNum(i)

  override def bottom: Element = (PInf, MInf)

  override def lub(x: Element, y: Element) = {
    (x, y) match {
      case ((MInf, PInf), _) => (MInf, PInf)
      case ((PInf, MInf), a) => a
      case ((MInf, _), (_, PInf)) => (MInf, PInf)
      case ((MInf, IntNum(h1)), (_, IntNum(h2))) => (MInf, IntNum(math.max(h1, h2)))
      case ((IntNum(l1), PInf), (IntNum(l2), _)) => (IntNum(math.min(l1, l2)), PInf)
      case ((IntNum(l1), IntNum(h1)), (IntNum(l2), IntNum(h2))) => (IntNum(math.min(l1, l2)), IntNum(math.max(h1, h2)))
      case _ => lub(y, x)
    }
  }

  trait Num

  case class IntNum(i: Int) extends Num {
    override def toString = s"$i"
  }

  object PInf extends Num {
    override def toString = "+inf"
  }

  object MInf extends Num {
    override def toString = "-inf"
  }

  override def sum(a: Element, b: Element): Element = {
    val low = (a._1, b._1) match {
      case (_, MInf) | (MInf, _) => MInf
      case (_, PInf) | (PInf, _) => PInf
      case (IntNum(i), IntNum(j)) => IntNum(i + j)
    }
    val high = (a._2, b._2) match {
      case (_, PInf) | (PInf, _) => PInf
      case (_, MInf) | (MInf, _) => MInf
      case (IntNum(i), IntNum(j)) => IntNum(i + j)
    }
    (low, high)
  }

  override def sub(a: Element, b: Element): Element = sum(a, inv(b))

  override def div(a: Element, b: Element): Element = {
    (a, b) match {
      case ((PInf, _), _) => (PInf, MInf)
      case (_, (PInf, _)) => (PInf, MInf)
      case _ => {

        val sa = signs(a)
        val sb = signs(b)
        val sbNoZero = sb - 0

        val d = { (x: Int, y: Int) => x / y }
        val dr = { (x: Int, y: Int) =>
          if (x == 0)
            0
          else y / x
        }

        val arange = sbNoZero.map(s => opNum(a, s, d))
        val brange = sa.map(s => opNum(b, s, dr))

        (min(arange.map { x => x._1 }), max(arange.map { x => x._2 }))
      }
    }
  }

  def min(s: Set[Num]): Num = {
    if (s.isEmpty) PInf
    else {
      s.reduce { (a, b) =>
        (a, b) match {
          case (PInf, x) => x
          case (x, PInf) => x
          case (MInf, _) | (_, MInf) => MInf
          case (IntNum(x), IntNum(y)) => IntNum(math.min(x, y))
        }
      }
    }
  }

  def max(s: Set[Num]): Num = {
    if (s.isEmpty) MInf
    else {
      s.reduce { (a, b) =>
        (a, b) match {
          case (PInf, _) | (_, PInf) => PInf
          case (x, MInf) => x
          case (MInf, x) => x
          case (IntNum(x), IntNum(y)) => IntNum(math.max(x, y))
        }
      }
    }
  }

  private def signs(a: Element): Set[Int] = {
    a match {
      case (MInf, PInf) => Set(-1, 0, +1)
      case (MInf, IntNum(x)) => if (x > 0) Set(-1, 0, +1, x) else if (x == 0) Set(-1, 0) else Set(x, -1)
      case (IntNum(x), PInf) => if (x < 0) Set(x, -1, 0, +1) else if (x == 0) Set(0, +1, x) else Set(+1, x)
      case (IntNum(l), IntNum(h)) => Set(-1, +1, 0, l, h).filter { x => (x <= h && x >= l) }
    }
  }

  private def opNum(a: Element, b: Int, op: (Int, Int) => Int): Element = {
    a match {
      case (PInf, _) => (PInf, MInf)
      case (MInf, PInf) => (MInf, PInf)
      case (MInf, IntNum(x)) => if (b == 0) (0, 0) else if (b < 0) (op(x, b), PInf) else (MInf, op(x, b))
      case (IntNum(x), PInf) => if (b == 0) (0, 0) else if (b < 0) (MInf, op(x, b)) else (op(x, b), PInf)
      case (IntNum(x), IntNum(y)) => (min(Set(op(x, b), op(y, b))), max(Set(op(x, b), op(y, b))))
    }
  }

  override def prod(a: Element, b: Element): Element = {
    (a, b) match {
      case ((PInf, _), _) => (PInf, MInf)
      case (_, (PInf, _)) => (PInf, MInf)
      case _ => {
        val sa = signs(a)
        val sb = signs(b)
        val mult = { (x: Int, y: Int) => x * y }

        val arange = sb.map(s => opNum(a, s, mult))
        val brange = sa.map(s => opNum(b, s, mult))

        (min(arange.map { x => x._1 }), max(arange.map { x => x._2 }))
      }
    }
  }

  private def inv(b: Element): Element = {
    b match {
      case (MInf, PInf) => (MInf, PInf)
      case (PInf, MInf) => (PInf, MInf)
      case (IntNum(j), PInf) => (MInf, IntNum(-j))
      case (MInf, IntNum(j)) => (IntNum(-j), PInf)
      case (IntNum(l), IntNum(h)) => (IntNum(math.min(-h, -l)), IntNum(math.max(-h, -l)))
    }
  }

  override def eqq(a: Element, b: Element): Element = {
    (a, b) match {
      case ((MInf, PInf), _) => (MInf, PInf)
      case (_, (MInf, PInf)) => (MInf, PInf)
      case ((IntNum(l1), IntNum(h1)), (IntNum(l2), IntNum(h2))) =>
        if (l1 == h1 && h1 == l2 && l2 == h2)
          (IntNum(1), IntNum(1))
        else
          (IntNum(0), IntNum(1))
      case _ =>
        (IntNum(0), IntNum(1))
    }
  }

  override def gt(a: Element, b: Element): Element = {
    (a, b) match {
      case ((MInf, PInf), _) => (MInf, PInf)
      case (_, (MInf, PInf)) => (MInf, PInf)
      case ((IntNum(l1), IntNum(h1)), (IntNum(l2), IntNum(h2))) =>
        if (h1 < l2)
          (IntNum(1), IntNum(1))
        else if (h2 < l1)
          (IntNum(0), IntNum(0))
        else
          (IntNum(0), IntNum(1))
      case _ =>
        //TODO: We can be more precise here
        (IntNum(0), IntNum(1))
    }
  }
}