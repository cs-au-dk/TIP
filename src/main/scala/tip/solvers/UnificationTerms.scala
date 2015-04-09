package tip.solvers

/**
 * A generic term: either a variable ([[Var]]), a constructor [[Cons]]
 *
 */
sealed trait Term[A] {

  def fv: Set[Var[A]]

  def subst(v: Var[A], t: Term[A]): Term[A]
}

/**
 * A variable
 *
 */
trait Var[A] extends Term[A] {

  def fv: Set[Var[A]] = Set(this)

  def subst(v: Var[A], t: Term[A]): Term[A] = {
    if (v == this) t else this
  }
}

/**
 * An n-ary term constructor.
 * 0-ary constructors are constants
 *
 */
trait Cons[A] extends Term[A] {

  def arity: Int

  def args: Seq[Term[A]]

  def doMatch(obj: Term[A]): Boolean = {
    this.getClass == obj.getClass && arity == obj.asInstanceOf[Cons[A]].arity
  }
}
