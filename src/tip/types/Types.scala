package tip.types

import tip.ast._
import tip.solvers._
import tip.ast.AstNodeData._
import scala.language.implicitConversions

object TipType {

  /**
    * Implicitly converts any AstNode to its type variable.
    * For identifiers the type variable is associated with the declaration;
    * for any other kind of AST node the type variable is associated with the node itself.
    *
    * To novice Scala programmers:
    * The keyword `implicit` in front of this function definition enables "implicit conversion" from `AstNode` to `Var[TipType]`.
    * This means that whenever Scala finds something of type `AstNode` but needs something of type `Var[TipType]`, this
    * function will be invoked implicitly to make the conversion (provided that the function is imported).
    * For more information about implicit conversions in Scala, see [[https://docs.scala-lang.org/tour/implicit-conversions.html]].
    */
  implicit def ast2typevar(node: AstNode)(implicit declData: DeclarationData): Var[TipType] = {
    node match {
      case id: AIdentifier => TipVar(declData(id))
      case _ => TipVar(node)
    }
  }
}

/**
  * A type for a TIP variable or expression.
  */
sealed trait TipType

object TipTypeOps extends TermOps[TipType] {

  def makeAlpha(node: Var[TipType]): Var[TipType] = node match {
    case v: TipVar => TipAlpha(v.node.uid)
    case alpha: TipAlpha => alpha
  }

  def makeMu(v: Var[TipType], t: Term[TipType]): Mu[TipType] = TipMu(v, t)
}

/**
  * Int type.
  */
case class TipInt() extends TipType with Cons[TipType] {

  val args: List[Term[TipType]] = List()

  def subst(v: Var[TipType], t: Term[TipType]): Term[TipType] = this

  override def toString: String = "int"
}

/**
  * Function type.
  */
case class TipFunction(params: List[Term[TipType]], ret: Term[TipType]) extends TipType with Cons[TipType] {

  val args: List[Term[TipType]] = {
    ret :: params
  }

  def subst(v: Var[TipType], t: Term[TipType]): Term[TipType] = {
    TipFunction(params.map { p =>
      p.subst(v, t)
    }, ret.subst(v, t))
  }

  override def toString: String = s"(${params.mkString(",")}) -> $ret"
}

/**
  * Pointer reference type.
  */
case class TipRef(of: Term[TipType]) extends TipType with Cons[TipType] {

  val args: List[Term[TipType]] = {
    List(of)
  }

  def subst(v: Var[TipType], t: Term[TipType]): Term[TipType] = {
    TipRef(of.subst(v, t))
  }

  override def toString: String = s"&$of"
}

/**
  * Type variable for a program variable or expression.
  */
case class TipVar(node: AstNode) extends TipType with Var[TipType] {

  override def toString: String = s"[[$node]]"
}

/**
  * Fresh type variable, whose identity is uniquely determined by `x`.
  */
case class TipAlpha(x: Any) extends TipType with Var[TipType] {

  override def toString: String =
    s"\u03B1<$x>"
}

/**
  * Recursive type (only created when closing terms).
  */
case class TipMu(v: Var[TipType], t: Term[TipType]) extends TipType with Mu[TipType] {

  def subst(sv: Var[TipType], to: Term[TipType]): Term[TipType] = {
    if (sv == v) this else TipMu(v, t.subst(sv, to))
  }
}
