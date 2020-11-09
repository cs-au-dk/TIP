package tip.solvers

import tip.util.Log

import scala.collection.mutable

/**
  * Unification solver based on union-find.
  * @tparam A type parameter describing the kind of constraint system
  */
class UnionFindSolver[A] {

  val log = Log.logger[this.type]()

  /**
    * This map holds the graph structure for the union-find algorithm.
    * Each term has a "parent". Two terms are equivalent (unified) if one is reachable from the other along zero or more parent links.
    * The parent of a term may be the term itself, in which case it is the representative for its equivalence class.
    */
  private val parent = mutable.Map[Term[A], Term[A]]()

  /**
    * Performs the unification of the two terms `t1` and `t2`.
    * When unifying a variable and a non-variable term, the non-variable term has higher priority for becoming the representative.
    */
  def unify(t1: Term[A], t2: Term[A]): Unit = {
    log.verb(s"Unifying $t1 and $t2")

    mkSet(t1)
    mkSet(t2)
    val rep1 = find(t1)
    val rep2 = find(t2)

    if (rep1 == rep2) return

    (rep1, rep2) match {
      case (v1: Var[A], v2: Var[A]) =>
        mkUnion(v1, v2)
      case (v1: Var[A], t2: Term[A]) =>
        mkUnion(v1, t2)
      case (t1: Term[A], v2: Var[A]) =>
        mkUnion(v2, t1)
      case (f1: Cons[A], f2: Cons[A]) if f1.doMatch(f2) =>
        mkUnion(f1, f2)
        f1.args.zip(f2.args).foreach {
          case (a1, a2) =>
            log.verb(s"Unifying subterms $a1 and $a2")
            unify(a1, a2)
        }
      case (x, y) =>
        throw new UnificationFailure(s"Cannot unify $t1 and $t2 (with representatives $x and $y)")
    }
  }

  /**
    * Returns the canonical element of the equivalence class of the term `t`.
    * The term is added as a new equivalence class if it has not been encountered before.
    * Uses path compression.
    */
  def find(t: Term[A]): Term[A] = {
    mkSet(t)
    if (parent(t) != t)
      parent += t -> find(parent(t))
    parent(t)
  }

  /**
    * Perform the union of the equivalence classes of `t1` and `t2`, such that `t2` becomes the new canonical element.
    * We assume `t1` and `t2` to be distinct canonical elements.
    * This implementation does not use [[https://en.wikipedia.org/wiki/Disjoint-set_data_structure union-by-rank]].
    */
  private def mkUnion(t1: Term[A], t2: Term[A]): Unit =
    parent += t1 -> t2

  /**
    * Creates an equivalence class for the term `t`, if it does not exists already.
    */
  private def mkSet(t: Term[A]): Unit =
    if (!parent.contains(t))
      parent += (t -> t)

  /**
    * Returns the solution of the solver.
    * Note that the terms in the solution have not yet been closed, i.e. they may contain constraint variables.
    * @return a map associating to each variable the representative of its equivalence class
    */
  def solution(): Map[Var[A], Term[A]] =
    // for each constraint variable, find its canonical representative (using the variable itself as default)
    parent.keys.collect { case v: Var[A] => (v, find(v)) }.toMap.withDefault(v => v)

  /**
    * Returns all the unifications of the solution.
    * @return a map from representative to equivalence class
    */
  def unifications(): Map[Term[A], Traversable[Term[A]]] =
    parent.keys.groupBy(find).withDefault(Set(_))

  /**
    * Produces a string representation of the solution.
    */
  override def toString =
    solution().map(p => s"${p._1} = ${p._2}").mkString("\n")
}

/**
  * Exception thrown in case of unification failure.
  */
class UnificationFailure(message: String = null) extends RuntimeException(message)
