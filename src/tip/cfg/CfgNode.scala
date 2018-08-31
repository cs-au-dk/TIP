package tip.cfg

import tip.ast._

import scala.collection.mutable

object CfgNode {

  var lastUid: Int = 0

  def uid: Int = {
    lastUid += 1
    lastUid
  }
}

/**
  * Node in a control-flow graph.
  */
trait CfgNode {

  /**
    * Predecessors of the node.
    */
  def pred: mutable.Set[CfgNode]

  /**
    * Successors of the node.
    */
  def succ: mutable.Set[CfgNode]

  /**
    * Unique node ID.
    */
  def id: Int

  /**
    * The AST node contained by this node.
    */
  def data: AstNode

  override def equals(obj: scala.Any): Boolean =
    obj match {
      case o: CfgNode => o.id == this.id
      case _ => false
    }

  override def hashCode(): Int = id
}

/**
  * Node in a CFG representing a program statement.
  * The `data` field holds the statement, or in case of if/while instructions, the branch condition.
  */
case class CfgStmtNode(
  override val id: Int = CfgNode.uid,
  override val pred: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
  override val succ: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
  data: AstNode
) extends CfgNode {

  override def toString: String = s"[Stmt] $data"
}

/**
  * Node in a CFG representing a function call.
  * The `data` field holds the assignment statement where the right-hand-side is the function call.
  */
case class CfgCallNode(
  override val id: Int = CfgNode.uid,
  override val pred: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
  override val succ: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
  data: AAssignStmt
) extends CfgNode {

  override def toString: String = s"[Call] $data"
}

/**
  * Node in a CFG representing having returned from a function call.
  * The `data` field holds the assignment statement where the right-hand-side is the function call.
  */
case class CfgAfterCallNode(
  override val id: Int = CfgNode.uid,
  override val pred: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
  override val succ: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
  data: AAssignStmt
) extends CfgNode {

  override def toString: String = s"[AfterCall] $data"
}

/**
  * Node in a CFG representing the entry of a function.
  */
case class CfgFunEntryNode(
  override val id: Int = CfgNode.uid,
  override val pred: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
  override val succ: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
  data: AFunDeclaration
) extends CfgNode {

  override def toString: String = s"[FunEntry] $data"
}

/**
  * Node in a CFG representing the exit of a function.
  */
case class CfgFunExitNode(
  override val id: Int = CfgNode.uid,
  override val pred: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
  override val succ: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
  data: AFunDeclaration
) extends CfgNode {

  override def toString: String = s"[FunExit] $data"
}
