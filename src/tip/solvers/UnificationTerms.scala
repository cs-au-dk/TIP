package tip.solvers

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

  def subst(v: Var[A], t: Term[A]): Term[A] = {
    if (v == this) t else this
  }
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
  def arity = args.length

  lazy val fv: Set[Var[A]] = args.flatMap(_.fv).toSet

  /**
    * Checks whether the term `t` matches this term, meaning that it has the same constructor class and the same arity.
    */
  def doMatch(t: Term[A]): Boolean = {
    this.getClass == t.getClass && arity == t.asInstanceOf[Cons[A]].arity
  }
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

  override def toString: String = s"\u03bc $v.$t"
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
    * Constructor for fresh variables.
    * The identity of the variable is uniquely determined by `x`.
    */
  def makeAlpha(x: Var[A]): Var[A]

  /**
    * Closes the term by replacing each free variable with its value in the given environment.
    * Whenever a recursive type is detected, a [[Mu]] term is generated.
    * Remaining free variables are replaced by fresh variables that are implicitly universally quantified.
    *
    * @param t       the term to close
    * @param env     environment, map from variables to terms
    */
  def close(t: Term[A], env: Map[Var[A], Term[A]]): Term[A] = closeRec(t, env)

  /**
    * Closes the term by replacing each variable that appears as a subterm of with its value in the given environment.
    * Whenever a recursive type is detected, a [[Mu]] term is generated.
    *
    * @param t       the term to close
    * @param env     environment, map from variables to terms
    * @param visited the set of already visited variables (empty by default)
    */
  private def closeRec(t: Term[A], env: Map[Var[A], Term[A]], visited: Set[Var[A]] = Set()): Term[A] = {
    t match {
      case v: Var[A] =>
        if (!visited.contains(v) && env(v) != v) {
          // no cycle found, and the variable does not map to itself
          val cterm = closeRec(env(v), env, visited + v)
          val newV = makeAlpha(v)
          if (cterm.fv.contains(newV)) {
            // recursive term found, make a [[Mu]]
            makeMu(newV, cterm.subst(v, newV))
          } else
            cterm
        } else {
          // an unconstrained (i.e. universally quantified) variable
          makeAlpha(v)
        }
      case c: Cons[A] =>
        // substitute each free variable with its closed term
        c.fv.foldLeft(t: Term[A]) { (acc, v) =>
          acc.subst(v, closeRec(v, env, visited))
        }
      case m: Mu[A] =>
        makeMu(m.v, closeRec(m.t, env, visited))
    }
  }
}
