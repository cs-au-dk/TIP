package tip.types

import tip.ast.AstNode
import tip.solvers.{Cons, Term, Var}

import scala.collection.immutable
import scala.language.implicitConversions

object TipType {

  implicit def ast2term(node: AstNode): Var[TipType] = {
    TipVar(node)
  }
}

sealed trait TipType

case class TipInt() extends TipType with Cons[TipType] {

  override def toString: String = "Int"

  override def args: Seq[Term[TipType]] = List()

  override def arity: Int = 0

  override def fv: Set[Var[TipType]] = Set()

  override def subst(v: Var[TipType], t: Term[TipType]): Term[TipType] = this
}

case class TipFunction(params: immutable.Seq[Term[TipType]], ret: Term[TipType]) extends TipType with Cons[TipType] {

  def fv: Set[Var[TipType]] = ret.fv ++ params.foldLeft(Set[Var[TipType]]()) { (set: Set[Var[TipType]], arg: Term[TipType]) => arg.fv ++ set }

  def subst(v: Var[TipType], t: Term[TipType]): Term[TipType] = {
    TipFunction(params.map { p => p.subst(v, t) }, ret.subst(v, t))
  }

  override def toString: String = {
    s"(${params.mkString(",")}) --> $ret"
  }

  override def arity: Int = {
    params.length + 1
  }

  override def args: Seq[Term[TipType]] = {
    ret :: params.toList
  }
}

case class TipRef(of: Term[TipType]) extends TipType with Cons[TipType] {

  def fv: Set[Var[TipType]] = of.fv

  def subst(v: Var[TipType], t: Term[TipType]): Term[TipType] = {
    TipRef(of.subst(v, t))
  }

  override def arity: Int = 1

  override def args: Seq[Term[TipType]] = {
    List(of)
  }

  override def toString: String = s"&$of"
}

case class TipVar(node: AstNode) extends TipType with Var[TipType] {
  override def toString: String = {
    val repr = node.toString.replace("\n", "")
    val s = repr.substring(0, Math.min(repr.length, 10))
    s"<($s)>${node.offset}"
  }
}

case class TipAlpha(node: AstNode) extends TipType with Var[TipType] {
  override def toString: String = {
    s"\u03B1<$node>"
  }
}

/**
 * A recursive type.
 * Whenever a term is such that x = t[x], where x appears free in
 * t[x], then we represent it finitely as
 * mu x. t[x]
 * x is a binder in the term, and the copy rule holds
 *
 * mu x. t[x] == t [ mu x. t[x] ]
 *
 * @param v the recursion variable
 * @param t the recursion term
 */
case class Mu[A](v: Var[A], t: Term[A]) extends TipType with Cons[A] {

  override def args: Seq[Term[A]] = List(v, t)

  override def arity: Int = 2

  def fv: Set[Var[A]] = t.fv - v

  def subst(sv: Var[A], to: Term[A]): Term[A] = {
    if (sv == v) this else Mu(v, t.subst(sv, to))
  }

  override def toString: String = s"\u03bc $v.$t"
}
