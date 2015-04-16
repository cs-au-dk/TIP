package tip.lattices

import scala.collection.immutable

/**
 * A (semi-)lattice.
 */
trait Lattice {
  
  /**
   * The type of the elements of this lattice.
   */
  type Element

  /**
   * The bottom element of this lattice.
   */
  def bottom: Element

  /**
   * The least upper bound of x and y.
   */
  def lub(x: Element, y: Element): Element

  /**
   * Returns true whenever x <= y in the lattice.
   */
  def leq(x: Element, y: Element): Boolean
}

/**
 * The sign lattice
 */
object SignLattice extends Lattice {

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

  override def leq(x: Element, y: Element) = {
    x == Bottom || y == Top || x == y
  }
}

/**
 * The n-th product lattice made of l lattices.
 */
class UniformProductLattice[L <: Lattice](val l: L, n: Int) extends Lattice {

  type Element = List[l.Element]

  override def bottom: Element = List.fill(n)(l.bottom)

  override def lub(x: Element, y: Element) = {
    if (x.length != y.length)
      error
    (x zip y).map { case (xc, yc) => l.lub(xc, yc) }
  }

  override def leq(x: Element, y: Element) = {
    if (x.length != y.length)
      error
    (x zip y).foldLeft(true) { (a, p) => a && l.leq(p._1, p._2) }
  }

  private def error() = throw new IllegalArgumentException("products not of same length")
}

/**
 * The flat lattice made of element of X.
 * Top is greater than every other element, and Bottom is less than every other element.
 * No additional ordering is defined.
 */
class FlatLattice[X]() extends Lattice {

  sealed trait FlatElement

  case class FlatEl(el: X) extends FlatElement

  case class Top() extends FlatElement

  case class Bot() extends FlatElement
  
  type Element = FlatElement

  /**
   * Lift an element of X into an element of the flat lattice .
   */
  implicit def lift(a: X): Element = FlatEl(a)

  /**
   * Un-lift an element of the lattice to an element of X.
   * If the element is Top or Bot then IllegalArgumentException is thrown.
   */
  implicit def unlift(a: Element): X = a match {
    case FlatEl(n) => n
    case _ => throw new IllegalArgumentException(s"cannot unlift $a")
  }

  override def bottom: Element = Bot()

  override def lub(x: Element, y: Element) = {
    if (x == Bot() || y == Top() || x == y)
      y
    else if (y == Bot() || x == Top())
      x
    else
      Top()
  }

  override def leq(x: Element, y: Element) = {
    x == Bot() || y == Top() || x == y
  }
}

/**
 * The product lattice made by l1 and l2.
 */
class PairLattice[L1 <: Lattice, L2 <: Lattice](val l1: L1, val l2: L2) extends Lattice {

  type Element = (l1.Element, l2.Element)

  override def bottom: Element = (l1.bottom, l2.bottom)

  override def lub(x: Element, y: Element) = (l1.lub(x._1, y._1), l2.lub(x._2, y._2))

  override def leq(x: Element, y: Element) = l1.leq(x._1, y._1) && l2.leq(x._2, y._2)
}

/**
 * A lattice of maps from the set X to the lattice l.
 * The set X a subset of A and it is defined by the characteristic function ch, i.e. 
 * a is in X if and only if ch(a) returns true.
 */
class MapLattice[A, +L <: Lattice](ch: A => Boolean, val l: L) extends Lattice {

  type Element = Map[A, l.Element]

  /**
   * Check whether x is an element of this lattice, i.e. whether its keys are in X.
   */
  private def checkDomain(x: Element): Unit = {
    if (!x.keys.forall { a => ch(a) })
      error(x.keys)
  }

  override def bottom: Element =
    Map().withDefault(a => if (ch(a)) l.bottom else error(a))

  override def lub(x: Element, y: Element) = {
    checkDomain(x)
    checkDomain(y)
    val m1 = x.keys.foldLeft(new immutable.HashMap[A, l.Element])((m, a) => m + (a -> l.lub(getsome(x, a), getsome(y, a))))
    val m2 = y.keys.foldLeft(m1)((m, a) => m + (a -> l.lub(getsome(x, a), getsome(y, a))))
    m2.withDefaultValue(l.bottom)
  }

  override def leq(x: Element, y: Element) = {
    checkDomain(x)
    checkDomain(y)
    x.keys.forall { (a) => l.leq(getsome(x, a), getsome(y, a)) } &&
      y.keys.forall { (a) => l.leq(getsome(x, a), getsome(y, a)) }
  }

  /**
   * Returns a new map of the lattice, 
   * where the element a is assigned to f, 
   * and all the other elements are mapped as in e.
   */
  def set(e: Element, a: A, f: l.Element): Element = {
    checkDomain(e)
    e + (a -> f)
  }

  /**
   * Retrieve the value that the map z assign to a, 
   * if a is in the domain of x.
   * Throws an IllegalArgumentException otherwise.
   */
  def getsome(x: Element, a: A) = {
    checkDomain(x)
    if (ch(a))
      x(a)
    else
      error(a)
  }

  private def error(a: Any) = throw new IllegalArgumentException(s"$a not in set defined by $ch")
}

/**
 * The powerset lattice of X, where X is the subset of A 
 * defined by the characteristic function ch.
 */
class PowersetLattice[A](ch: A => Boolean) extends Lattice {

  type Element = Set[A]

  override def bottom: Element = ???

  override def lub(x: Element, y: Element) = ???

  override def leq(x: Element, y: Element) = ???

}

