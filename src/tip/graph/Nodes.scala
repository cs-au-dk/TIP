package tip.graph

import scala.collection.mutable


object GNode {
  var _uid = -1

  def uid = {
    _uid += 1
    _uid
  }
}

/**
 * Generic node in the CFG
 */
trait GNode[A] {
  def pred: mutable.Set[GNode[A]]

  def succ: mutable.Set[GNode[A]]

  def id: Int

  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case o: GNode[A] => o.id == this.id
      case _ => false
    }
  }

  override def hashCode(): Int = id

}

/**
 * Node in the CFG holding data
 */
case class GRealNode[A](
                         override val id: Int = GNode.uid,
                         override val pred: mutable.Set[GNode[A]] = mutable.Set[GNode[A]](),
                         override val succ: mutable.Set[GNode[A]] = mutable.Set[GNode[A]](),
                         data: A) extends GNode[A] {

  override def toString: String = data.toString
}

/**
 * Auxiliary node of the CFG, to easy building the CFG compositionally
 * They are supposed to be removed from the final CFG
 */
case class AuxNode[A](
                       override val id: Int = GNode.uid,
                       override val pred: mutable.Set[GNode[A]] = mutable.Set[GNode[A]](),
                       override val succ: mutable.Set[GNode[A]] = mutable.Set[GNode[A]]()) extends GNode[A] {

  override def toString: String = s"AuxNode-$id"

}

/**
 * A function call node
 */
case class CallNode[A](
                        override val id: Int = GNode.uid,
                        override val pred: mutable.Set[GNode[A]] = mutable.Set[GNode[A]](),
                        override val succ: mutable.Set[GNode[A]] = mutable.Set[GNode[A]](),
                        data: A) extends GNode[A] {

  override def toString: String = s"Call-$data"
}

/**
 * A function after-call node
 */
case class AfterCallNode[A](
                             override val id: Int = GNode.uid,
                             override val pred: mutable.Set[GNode[A]] = mutable.Set[GNode[A]](),
                             override val succ: mutable.Set[GNode[A]] = mutable.Set[GNode[A]](),
                             data: A) extends GNode[A] {

  override def toString: String = s"AfterCall-$data"
}

/**
 * The entry node of a function
 */

case class FunEntry[A](
                        override val id: Int = GNode.uid,
                        override val pred: mutable.Set[GNode[A]] = mutable.Set[GNode[A]](),
                        override val succ: mutable.Set[GNode[A]] = mutable.Set[GNode[A]](),
                        data: A) extends GNode[A] {

  override def toString: String = s"FunEntry-$data"
}

/**
 * The exit node of a function
 */
case class FunExit[A](
                       override val id: Int = GNode.uid,
                       override val pred: mutable.Set[GNode[A]] = mutable.Set[GNode[A]](),
                       override val succ: mutable.Set[GNode[A]] = mutable.Set[GNode[A]](),
                       data: A) extends GNode[A] {

  override def toString: String = s"FunExit-$data"
}