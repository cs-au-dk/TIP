package tip.lattices

import tip.ast._
import tip.ast.AstNodeData.{AstNodeWithDeclaration, DeclarationData}

/**
  * An element of the sign lattice.
  */
object SignElement extends Enumeration {
  val Pos, Neg, Zero = Value
}

/**
  * The sign lattice.
  */
object SignLattice extends FlatLattice[SignElement.Value] with LatticeOps {

  import SignElement._

  override def lub(x: Element, y: Element) = {
    if (x == Bot || y == Top || x == y)
      y
    else if (y == Bot || x == Top)
      x
    else
      Top
  }

  private val signValues: Map[FlatElement, Int] = Map(Bot -> 0, FlatEl(Zero) -> 1, FlatEl(Neg) -> 2, FlatEl(Pos) -> 3, Top -> 4)

  private def abs(op: List[List[SignLattice.Element]], x: SignLattice.Element, y: SignLattice.Element): SignLattice.Element = {
    op(signValues(x))(signValues(y))
  }

  private val absPlus: List[List[FlatElement]] = List(
    List(Bot, Bot, Bot, Bot, Bot),
    List(Bot, Zero, Neg, Pos, Top),
    List(Bot, Neg, Neg, Top, Top),
    List(Bot, Pos, Top, Pos, Top),
    List(Bot, Top, Top, Top, Top)
  )

  private val absMinus: List[List[FlatElement]] = List(
    List(Bot, Bot, Bot, Bot, Bot),
    List(Bot, Zero, Pos, Neg, Top),
    List(Bot, Neg, Top, Neg, Top),
    List(Bot, Pos, Pos, Top, Top),
    List(Bot, Top, Top, Top, Top)
  )

  private val absTimes: List[List[FlatElement]] = List(
    List(Bot, Bot, Bot, Bot, Bot),
    List(Bot, Zero, Zero, Zero, Zero),
    List(Bot, Zero, Pos, Neg, Top),
    List(Bot, Zero, Neg, Pos, Top),
    List(Bot, Zero, Top, Top, Top)
  )

  private val absDivide: List[List[FlatElement]] = List(
    List(Bot, Bot, Bot, Bot, Bot),
    List(Bot, Top, Zero, Zero, Top),
    List(Bot, Top, Top, Top, Top),
    List(Bot, Top, Top, Top, Top),
    List(Bot, Top, Top, Top, Top)
  )

  private val absGt: List[List[FlatElement]] = List(
    List(Bot, Bot, Bot, Bot, Bot),
    List(Bot, Zero, Pos, Zero, Top),
    List(Bot, Zero, Top, Zero, Top),
    List(Bot, Pos, Pos, Top, Top),
    List(Bot, Top, Top, Top, Top)
  )

  private val absEq: List[List[FlatElement]] =
    List(
      List(Bot, Bot, Bot, Bot, Bot),
      List(Bot, Pos, Zero, Zero, Top),
      List(Bot, Zero, Top, Zero, Top),
      List(Bot, Zero, Zero, Top, Top),
      List(Bot, Top, Top, Top, Top)
    )

  override def plus(a: SignLattice.Element, b: SignLattice.Element) = abs(absPlus, a, b)

  override def minus(a: SignLattice.Element, b: SignLattice.Element) = abs(absMinus, a, b)

  override def times(a: SignLattice.Element, b: SignLattice.Element) = abs(absTimes, a, b)

  override def div(a: SignLattice.Element, b: SignLattice.Element) = abs(absDivide, a, b)

  override def eqq(a: SignLattice.Element, b: SignLattice.Element) = abs(absEq, a, b)

  override def gt(a: SignLattice.Element, b: SignLattice.Element) = abs(absGt, a, b)

  /**
    * Returns the sign of `i`.
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
    * Evaluates the expression `exp` in the abstract domain of signs, using `env` as the current environment.
    */
  def eval[A](exp: AExpr, env: Map[ADeclaration, Element])(implicit declData: DeclarationData): Element = {
    exp match {
      case id: AIdentifier => env(id.declaration)
      case num: ANumber => sign(num.value)
      case bin: ABinaryOp =>
        bin.operator match {
          case Plus =>
            plus(eval(bin.left, env), eval(bin.right, env))
          case Minus =>
            minus(eval(bin.left, env), eval(bin.right, env))
          case Times =>
            times(eval(bin.left, env), eval(bin.right, env))
          case Divide =>
            div(eval(bin.left, env), eval(bin.right, env))
          case GreatThan =>
            gt(eval(bin.left, env), eval(bin.right, env))
          case Eqq =>
            eqq(eval(bin.left, env), eval(bin.right, env))
          case _ => ???
        }
      case _: AInput => Top
      case _ => ???
    }
  }
}
