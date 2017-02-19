package tip.lattices

/**
  * Abstract operators.
  */
trait LatticeOps {

  type Element

  def plus(a: Element, b: Element): Element

  def minus(a: Element, b: Element): Element

  def times(a: Element, b: Element): Element

  def div(a: Element, b: Element): Element

  def eqq(a: Element, b: Element): Element

  def gt(a: Element, b: Element): Element
}
