package tip.lattices

import tip.solvers.Lambda

/**
  * Lattice of the form ((D -> L) -> (D -> L)).
  * Each element is represented by Map[(DL, DL), edgelattice.Element]
  * using the bottom element of `EdgeFunctionLattice` as default value.
  */
class EdgeEnvLattice[D, L <: Lattice, EFL <: EdgeFunctionLattice[L]](val edgelattice: EFL) extends Lattice {

  import edgelattice.valuelattice

  type DL = Either[D, Lambda]

  type Element = Map[(DL, DL), edgelattice.Element]

  type StateLatticeElement = Map[D, valuelattice.Element]

  val bottom: Element = Map()

  def lub(x: Element, y: Element): Element =
    y.foldLeft(x) { case (tm, (dd, ye)) => tm + (dd -> edgelattice.lub(tm.getOrElse(dd, edgelattice.bottom), ye)) }

  /**
    * Applies the function f: (D -> L) -> (D -> L) to the value x: D -> L
    * where f is represented by Map[(DL, DL), edgelattice.Element].
    */
  def apply(f: Element, x: StateLatticeElement): StateLatticeElement =
    apply(curry(f), x)

  /**
    * Applies the function f: (D -> L) -> (D -> L) to the value x: D -> L
    * where f is represented by DL => Map[DL, edgelattice.Element].
    */
  def apply(f: DL => Map[DL, edgelattice.Element], x: StateLatticeElement): StateLatticeElement = {
    val t1 = x.toList.flatMap {
      case (a, v) =>
        f(Left(a)).toList.flatMap {
          case (Left(b), e) => Some(b, e(v))
          case _ => None
        }
    }
    val t2 = f(Right(Lambda())).toList.flatMap {
      case (Left(b), e) => Some(b, e(valuelattice.bottom))
      case _ => None
    }
    build2(t1 ++ t2)
  }

  /**
    * Composes f and g.
    * The resulting element first applies `f` then `g`.
    */
  def compose(f: Element, g: Element): Element =
    compose2(f, curry(g))

  /**
    * Composes f and g.
    * The resulting element first applies `f` then `g`.
    */
  def compose2(f: Element, g: DL => Map[DL, edgelattice.Element]): Element =
    build(f.toList.flatMap { case ((d1, d2), fe) => g(d2).map { case (d3, ge) => ((d1, d3), ge.composeWith(fe)) } })

  /**
    * Builds a Element from a list of key-value pairs.
    */
  private def build(q: List[((DL, DL), edgelattice.Element)]): Element =
    q.foldLeft(Map[(DL, DL), edgelattice.Element]()) { case (tm, (dd, e)) => tm + (dd -> edgelattice.lub(tm.getOrElse(dd, edgelattice.bottom), e)) }

  /**
    * Builds a StateLatticeElement from a list of key-value pairs.
    */
  private def build2(q: List[(D, valuelattice.Element)]): StateLatticeElement =
    q.foldLeft(Map[D, valuelattice.Element]()) { case (tm, (d, v)) => tm + (d -> valuelattice.lub(tm.getOrElse(d, valuelattice.bottom), v)) }
      .withDefaultValue(valuelattice.bottom)

  /**
    * Transforms from Map[(A, B), C] to Map[A, Map[B, C].
    */
  private def curry[A, B, C](m: Map[(A, B), C]): Map[A, Map[B, C]] =
    m.foldLeft(Map[A, Map[B, C]]()) { case (t, ((a, b), c)) => t + (a -> (t.getOrElse(a, Map()) + (b -> c))) }.withDefaultValue(Map())
}
