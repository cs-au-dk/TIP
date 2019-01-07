package tip.lattices

/**
  * Lattice with abstract operators.
  */
trait LatticeWithOps extends Lattice {

  /**
    * Abstract number.
    */
  def num(i: Int): Element

  /**
    * Abstract plus.
    */
  def plus(a: Element, b: Element): Element

  /**
    * Abstract minus.
    */
  def minus(a: Element, b: Element): Element

  /**
    * Abstract times.
    */
  def times(a: Element, b: Element): Element

  /**
    * Abstract division.
    */
  def div(a: Element, b: Element): Element

  /**
    * Abstract equals.
    */
  def eqq(a: Element, b: Element): Element

  /**
    * Abstract greater-than.
    */
  def gt(a: Element, b: Element): Element
}
