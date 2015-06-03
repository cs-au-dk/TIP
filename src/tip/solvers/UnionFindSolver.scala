package tip.solvers

import tip.logging.Log

import scala.collection.{immutable, mutable}

class UnionFindSolver[A] {

  val log = Log.typeLogger[this.type](Log.Level.Debug)

  /**
   * This map holds the graph structure for the union-find algorithm
   */
  private val parent = mutable.Map[Term[A], Term[A]]()

  /**
   * Returns the solution of the solver
   *
   * @return a map associating to each variable the computed solution
   */
  def solution(): Map[Var[A], Term[A]] = {
    parent.keys.filter(_.isInstanceOf[Var[A]]).
      map { v => v.asInstanceOf[Var[A]] -> find(v) }.toMap.withDefault(v => v)
  }

  /**
   * Returns all the unifications of the solution
   *
   * @return a map associating to each term the terms unified with it
   */
  def unifications(): immutable.Map[Term[A], Set[Term[A]]] = {
    parent.keys.map { t => t -> parent.keys.filter { x => find(t) == find(x) }.toSet }.toMap.withDefault(Set(_))
  }


  /**
   * Performs the unification of the term `b1` and `b2`
   *
   * @param b1 the right side of the equality
   * @param b2 the left side of the equality
   */
  def unify(b1: Term[A], b2: Term[A]): Unit = {

    log.debug(s"Unifying: $b1 = $b2")

    mkSet(b1)
    mkSet(b2)
    val rep1 = find(b1)
    val rep2 = find(b2)

    if (rep1 == rep2) return

    (rep1, rep2) match {
      case (v1: Var[A], v2: Var[A]) =>
        mkUnion(v1, v2)
      case (v1: Var[A], t2: Term[A]) =>
        mkUnion(v1, t2)
      case (t1: Term[A], v2: Var[A]) =>
        mkUnion(v2, t1)
      case (f1: Cons[A], f2: Cons[A]) =>
        if (!f1.doMatch(f2)) throw new UnificationFailure(s"$f1 != $f2")
        //otherwise
        mkUnion(f1, f2)
        f1.args.zip(f2.args).foreach {
          case (a1, a2) =>
            unify(a1, a2)
        }
      case (t1, t2) =>
        throw new UnificationFailure(s"$t1 != $t2")
    }
  }

  /**
   * Returns the canonical element of the equivalence class of the term `t`
   *
   * @param t the element of which we seek the canonical element
   * @return the canonical element
   */
  private def find(t: Term[A]): Term[A] = {
    val k1 = parent(t)
    if (k1 == t) t else find(k1)
  }

  /**
   * Perform the union of the equivalence classes
   * of `t1` and `t2`, `t2` becomes the new canonical element.
   * We assume `t1` and `t2` to be canonical elements.
   *
   * @param t1 the canonical element representing one of the equivalence classes to unify
   * @param t2 the canonical element representing one of the equivalence classes to unify, the element will become the canonical element of the union
   */
  private def mkUnion(t1: Term[A], t2: Term[A]): Unit = {
    parent += (t1 -> t2)
  }

  /**
   * Creates an equivalence class for the term `t`,
   * if it does not exists already
   *
   * @param t the term for which we wish to make a new equivalence class
   */
  private def mkSet(t: Term[A]): Unit = {
    if (!parent.contains(t))
      parent += (t -> t);
  }

  override def toString = {
    solution.map(p => s"${p._1} = ${p._2}").mkString("\n")
  }
}

class UnificationFailure(message: String = null, cause: Throwable = null) extends RuntimeException(message, cause)
