package tip.lattices

import scala.language.implicitConversions

/**
  * A (semi-)lattice.
  */
trait Lattice {

  /**
    * The type of the elements of this lattice.
    *
    * To novice Scala programmers:
    * This is an example of an abstract type member. In this trait, `Element` is just a name for a type.
    * It is constrained in sub-traits and sub-classes, similarly to type parameters in generic classes.
    * For more information about abstract type members in Scala, see [[https://docs.scala-lang.org/tour/abstract-types.html]].
    */
  type Element

  /**
    * The bottom element of this lattice.
    */
  val bottom: Element

  /**
    * The top element of this lattice.
    * Default: not implemented.
    */
  def top: Element = ???

  /**
    * The least upper bound of `x` and `y`.
    */
  def lub(x: Element, y: Element): Element

  /**
    * Returns true whenever `x` <= `y`.
    */
  def leq(x: Element, y: Element): Boolean = lub(x, y) == y // rarely used, but easy to implement :-)
}

/**
  * The `n`-th product lattice made of `sublattice` lattices.
  */
class UniformProductLattice[L <: Lattice](val sublattice: L, n: Int) extends Lattice {

  type Element = List[sublattice.Element]

  val bottom: Element = List.fill(n)(sublattice.bottom)

  def lub(x: Element, y: Element): Element = {
    if (x.length != y.length)
      error()
    (x zip y).map { case (xc, yc) => sublattice.lub(xc, yc) }
  }

  private def error() = throw new IllegalArgumentException("Products not of same length")
}

/**
  * The flat lattice made of element of `X`.
  * Top is greater than every other element, and Bottom is less than every other element.
  * No additional ordering is defined.
  */
class FlatLattice[X] extends Lattice {

  sealed trait FlatElement

  case class FlatEl(el: X) extends FlatElement {
    override def toString = el.toString
  }

  final case object Top extends FlatElement {
    override def toString = "Top"
  }

  final case object Bot extends FlatElement {
    override def toString = "Bot"
  }

  type Element = FlatElement

  /**
    * Wrap an element of `X` into an element of the flat lattice.
    */
  implicit def wrap(a: X): Element = FlatEl(a)

  /**
    * Unwrap an element of the lattice to an element of `X`.
    * If the element is Top or Bot then IllegalArgumentException is thrown.
    * Note that this method is declared as implicit, so the conversion can be done automatically.
    */
  implicit def unwrap(a: Element): X = a match {
    case FlatEl(n) => n
    case _ => throw new IllegalArgumentException(s"Cannot unlift $a")
  }

  val bottom: Element = Bot

  override val top: Element = Top

  def lub(x: Element, y: Element): Element =
    if (x == Bot || y == Top || x == y)
      y
    else if (y == Bot || x == Top)
      x
    else
      Top
}

/**
  * The two-element lattice containing only Top and Bot.
  */
class TwoElementLattice extends FlatLattice[Nothing]

/**
  * The product lattice made by `l1` and `l2`.
  */
class PairLattice[L1 <: Lattice, L2 <: Lattice](val sublattice1: L1, val sublattice2: L2) extends Lattice {

  type Element = (sublattice1.Element, sublattice2.Element)

  val bottom: Element = (sublattice1.bottom, sublattice2.bottom)

  def lub(x: Element, y: Element): Element = (sublattice1.lub(x._1, y._1), sublattice2.lub(x._2, y._2))
}

/**
  * A lattice of maps from a set of elements of type `A` to the lattice `sublattice`.
  * Bottom is the default value.
  */
class MapLattice[A, +L <: Lattice](val sublattice: L) extends Lattice {

  type Element = Map[A, sublattice.Element]

  val bottom: Element = Map().withDefaultValue(sublattice.bottom)

  def lub(x: Element, y: Element): Element =
    x.keys.foldLeft(y)((m, a) => m + (a -> sublattice.lub(x(a), y(a)))).withDefaultValue(sublattice.bottom)
}

/**
  * The powerset lattice of a set of elements of type `A` with subset ordering.
  */
class PowersetLattice[A] extends Lattice {

  type Element = Set[A]

  val bottom: Element = ??? //<--- Complete here

  def lub(x: Element, y: Element): Element = ??? //<--- Complete here
}

/**
  * The powerset lattice of the given set of elements of type `A` with superset ordering.
  */
class ReversePowersetLattice[A](s: Set[A]) extends Lattice {

  type Element = Set[A]

  val bottom: Element = s

  def lub(x: Element, y: Element): Element = x intersect y
}

/**
  * The lift lattice for `sublattice`.
  * Supports implicit lifting and unlifting.
  */
class LiftLattice[+L <: Lattice](val sublattice: L) extends Lattice {

  type Element = Lifted

  sealed trait Lifted

  case object Bottom extends Lifted {
    override def toString = "LiftBot"
  }

  case class Lift(n: sublattice.Element) extends Lifted

  val bottom: Element = Bottom

  def lub(x: Element, y: Element): Element =
    (x, y) match {
      case (Bottom, t) => t
      case (t, Bottom) => t
      case (Lift(a), Lift(b)) => Lift(sublattice.lub(a, b))
    }

  /**
    * Lift elements of the sublattice to this lattice.
    * Note that this method is declared as implicit, so the conversion can be done automatically.
    */
  implicit def lift(x: sublattice.Element): Element = Lift(x)

  /**
    * Un-lift elements of this lattice to the sublattice.
    * Throws an IllegalArgumentException if trying to unlift the bottom element
    * Note that this method is declared as implicit, so the conversion can be done automatically.
    */
  implicit def unlift(x: Element): sublattice.Element = x match {
    case Lift(s) => s
    case Bottom => throw new IllegalArgumentException("Cannot unlift bottom")
  }
}
