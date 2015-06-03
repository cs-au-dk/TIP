package tip.lattices

/**
 * Abstract operators.
 */
trait LatticeOps {

  type Element

  def sum(a: Element, b: Element): Element

  def sub(a: Element, b: Element): Element

  def prod(a: Element, b: Element): Element

  def div(a: Element, b: Element): Element

  def eqq(a: Element, b: Element): Element

  def gt(a: Element, b: Element): Element
}
