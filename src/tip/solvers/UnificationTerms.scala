package tip.solvers

import scala.collection.mutable

/**
  * A generic term: a variable [[Var]], a constructor [[Cons]], or a recursive term [[Mu]].
  *
  * @tparam A type parameter describing the kind of constraint system
  */
sealed trait Term[A] {

  /**
    * Returns the set of free variables in this term.
    */
  def fv: Set[Var[A]]

  /**
    * Produces a new term from this term by substituting the variable `v` with the term `t`.
    */
  def subst(v: Var[A], t: Term[A]): Term[A]
}

/**
  * A constraint variable.
  */
trait Var[A] extends Term[A] {

  val fv: Set[Var[A]] = Set(this)

  def subst(v: Var[A], t: Term[A]): Term[A] =
    if (v == this) t else this
}

/**
  * An n-ary term constructor.
  * 0-ary constructors are constants.
  */
trait Cons[A] extends Term[A] {

  /**
    * The sub-terms.
    */
  val args: List[Term[A]]

  /**
    * The arity of the constructor.
    */
  def arity: Int = args.length

  lazy val fv: Set[Var[A]] = args.flatMap(_.fv).toSet

  /**
    * Checks whether the term `t` matches this term, meaning that it has the same constructor class and the same arity.
    */
  def doMatch(t: Term[A]): Boolean =
    this.getClass == t.getClass && arity == t.asInstanceOf[Cons[A]].arity
}

/**
  * Recursive term.
  * Whenever a term is such that v = t[v] where v appears free in t[v], then we represent it finitely as \u03bc v. t[v].
  * v is a binder in the term, and the copy rule holds: \u03bc v. t[v] == t [ \u03bc v. t[v] ]
  */
trait Mu[A] extends Term[A] {

  /**
    * The variable.
    */
  val v: Var[A]

  /**
    * The term.
    */
  val t: Term[A]

  lazy val fv: Set[Var[A]] = t.fv - v

  override def toString: String = s"\u03bc$v.$t"
}

/**
  * Special operations on terms.
  */
trait TermOps[A] {

  /**
    * Constructor for [[tip.solvers.Mu]] terms.
    */
  def makeMu(v: Var[A], t: Term[A]): Mu[A]

  /**
    * Constructor for fresh term variables.
    */
  def makeFreshVar(): Var[A]

  /**
    * Closes the term by replacing each free variable with its value in the given environment.
    * Whenever a recursive term is detected, a [[Mu]] term is generated.
    * Remaining free variables are replaced by fresh variables that are implicitly universally quantified.
    *
    * @param t         the term to close
    * @param env       environment, map from term variables to terms
    * @param freshvars map from recursive and unconstrained term variables to fresh term variables
    */
  def close(t: Term[A], env: Map[Var[A], Term[A]], freshvars: mutable.Map[Var[A], Var[A]]): Term[A] = {

    def closeRec(t: Term[A], visited: Set[Var[A]] = Set()): Term[A] =
      t match {
        case v: Var[A] =>
          if (!visited.contains(v) && env(v) != v) {
            // no recursion found, and the variable does not map to itself
            val cterm = closeRec(env(v), visited + v)
            val f = freshvars.get(v)
            if (f.isDefined && cterm.fv.contains(f.get)) {
              // recursive term found, make a [[Mu]]
              makeMu(f.get, cterm.subst(v, f.get))
            } else
              cterm
          } else {
            // recursive or unconstrained term variables, make a fresh term variable
            freshvars.getOrElse(v, {
              val w = makeFreshVar()
              freshvars += v -> w
              w
            })
          }
        case c: Cons[A] =>
          // substitute each free variable with its closed term
          c.fv.foldLeft(t) { (acc, v) =>
            acc.subst(v, closeRec(v, visited))
          }
        case m: Mu[A] =>
          makeMu(m.v, closeRec(m.t, visited))
      }

    closeRec(t)
  }
}
