package tip.lattices

import scala.language.implicitConversions

/**
  * The interval lattice.
  */
object IntervalLattice extends LatticeWithOps {

  /**
    * The element of the IntervalLattice.
    * An interval of the form (x, y) ranges from x to y (x < y), x and y included.
    * The interval (PInf, MInf) is the canonical empty interval, i.e. the bottom element.
    * The interval (MInf, PInf) is the top element.
    */
  type Element = (Num, Num)

  val FullInterval: Element = (MInf, PInf)

  val EmptyInterval: Element = (PInf, MInf)

  implicit def int2num(i: Int): IntNum = IntNum(i)

  val bottom: Element = EmptyInterval

  override def top: Element = FullInterval

  def lub(x: Element, y: Element): Element =
    (x, y) match {
      case (FullInterval, _) => FullInterval
      case (EmptyInterval, a) => a
      case ((MInf, _), (_, PInf)) => FullInterval
      case ((MInf, IntNum(h1)), (_, IntNum(h2))) => (MInf, IntNum(math.max(h1, h2)))
      case ((IntNum(l1), PInf), (IntNum(l2), _)) => (IntNum(math.min(l1, l2)), PInf)
      case ((IntNum(l1), IntNum(h1)), (IntNum(l2), IntNum(h2))) => (IntNum(math.min(l1, l2)), IntNum(math.max(h1, h2)))
      case _ => lub(y, x)
    }

  /**
    * A Num is an int, +infinity, or -infinity.
    */
  sealed trait Num extends Ordered[Num] {
    def compare(that: Num): Int =
      (this, that) match {
        case (x, y) if x == y => 0
        case (IntNum(a), IntNum(b)) => a - b
        case (MInf, _) => -1
        case (_, PInf) => -1
        case (PInf, _) => 1
        case (_, MInf) => 1
      }
  }

  case class IntNum(i: Int) extends Num {
    override def toString = s"$i"
  }

  case object PInf extends Num {
    override def toString = "+inf"
  }

  case object MInf extends Num {
    override def toString = "-inf"
  }

  /**
    * Number as interval.
    */
  def num(i: Int): Element = (IntNum(i), IntNum(i))

  /**
    * Abstract binary `+` on intervals.
    */
  def plus(a: Element, b: Element): Element = {
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

  /**
    * Abstract binary `-` on intervals.
    */
  def minus(a: Element, b: Element): Element = plus(a, inv(b))

  /**
    * Abstract `/` on intervals.
    */
  def div(a: Element, b: Element): Element =
    (a, b) match {
      case ((PInf, _), _) => EmptyInterval
      case (_, (PInf, _)) => EmptyInterval
      case _ =>
        val sb = signs(b)
        val sbNoZero = sb - 0
        val d = { (x: Int, y: Int) =>
          x / y
        }
        val arange = sbNoZero.map(s => opNum(a, s, d))
        (min(arange.map { x =>
          x._1
        }), max(arange.map { x =>
          x._2
        }))
    }

  /**
    * Finds the minimum of the given set of Num values.
    */
  def min(s: Set[Num]): Num =
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

  /**
    * Finds the maximum of the given set of Num values.
    */
  def max(s: Set[Num]): Num =
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

  /**
    * Returns the set of signs of the integers in the given interval.
    */
  private def signs(a: Element): Set[Int] =
    a match {
      case (MInf, PInf) => Set(-1, 0, +1)
      case (MInf, IntNum(x)) => if (x > 0) Set(-1, 0, +1, x) else if (x == 0) Set(-1, 0) else Set(x, -1)
      case (IntNum(x), PInf) => if (x < 0) Set(x, -1, 0, +1) else if (x == 0) Set(0, +1, x) else Set(+1, x)
      case (IntNum(l), IntNum(h)) =>
        Set(-1, +1, 0, l, h).filter { x =>
          x <= h && x >= l
        }
      case (MInf, MInf) => Set(-1)
      case (PInf, PInf) => Set(+1)
      case _ => ???
    }

  /**
    * Apples the binary operator `op` on the interval `a` and the int `b`.
    */
  private def opNum(a: Element, b: Int, op: (Int, Int) => Int): Element =
    a match {
      case (PInf, _) => EmptyInterval
      case (_, MInf) => EmptyInterval
      case (MInf, PInf) => FullInterval
      case (MInf, IntNum(x)) => if (b == 0) (0, 0) else if (b < 0) (op(x, b), PInf) else (MInf, op(x, b))
      case (IntNum(x), PInf) => if (b == 0) (0, 0) else if (b < 0) (MInf, op(x, b)) else (op(x, b), PInf)
      case (IntNum(x), IntNum(y)) => (min(Set(op(x, b), op(y, b))), max(Set(op(x, b), op(y, b))))
    }

  /**
    * Abstract `*` on intervals;
    */
  def times(a: Element, b: Element): Element =
    (a, b) match {
      case ((PInf, _), _) => EmptyInterval
      case (_, (PInf, _)) => EmptyInterval
      case _ =>
        val sa = signs(a)
        val sb = signs(b)
        val mult = { (x: Int, y: Int) =>
          x * y
        }
        val arange = sb.map(s => opNum(a, s, mult))
        val brange = sa.map(s => opNum(b, s, mult))
        (min(arange.map { x =>
          x._1
        }), max(brange.map { x =>
          x._2
        }))
    }

  /**
    * Abstract unary `-` on intervals.
    */
  private def inv(b: Element): Element =
    b match {
      case (MInf, PInf) => FullInterval
      case (PInf, MInf) => EmptyInterval
      case (IntNum(j), PInf) => (MInf, IntNum(-j))
      case (MInf, IntNum(j)) => (IntNum(-j), PInf)
      case (IntNum(l), IntNum(h)) => (IntNum(math.min(-h, -l)), IntNum(math.max(-h, -l)))
      case (MInf, MInf) => (PInf, PInf)
      case (PInf, PInf) => (MInf, MInf)
      case _ => ???
    }

  /**
    * Abstract `==` on intervals;
    */
  def eqq(a: Element, b: Element): Element =
    (a, b) match {
      case (FullInterval, _) => FullInterval
      case (_, FullInterval) => FullInterval
      case ((IntNum(l1), IntNum(h1)), (IntNum(l2), IntNum(h2))) =>
        if (l1 == h1 && h1 == l2 && l2 == h2)
          (IntNum(1), IntNum(1))
        else
          (IntNum(0), IntNum(1))
      case _ =>
        (IntNum(0), IntNum(1))
    }

  /**
    * Abstract `>` on intervals;
    */
  def gt(a: Element, b: Element): Element =
    (a, b) match {
      case (FullInterval, _) => FullInterval
      case (_, FullInterval) => FullInterval
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
