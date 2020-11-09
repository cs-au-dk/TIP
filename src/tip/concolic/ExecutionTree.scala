package tip.concolic

import tip.ast.AExpr
import tip.concolic.ExecutionTree._
import tip.util.Log

import scala.collection.mutable

object ExecutionTree {
  val log = Log.logger[this.type]()
}

/**
  * Execution tree node
  */
sealed trait ExecutionTree { self =>

  def parent: ExecutionTree

  def pathCondition(suffix: List[(AExpr, Boolean)] = Nil): List[(AExpr, Boolean)] =
    parent match {
      case b: Branch if b.branches(true) == self =>
        // self node is in the true branch
        (b.symcondition, true) :: b.pathCondition(suffix)
      case b: Branch if b.branches(false) == self =>
        // self node is in the false branch
        (b.symcondition, false) :: b.pathCondition(suffix)
      case _ => parent.pathCondition(suffix)
    }

  def children: List[ExecutionTree]

  def branch(cond: AExpr, symcond: AExpr, value: Boolean): ExecutionTree

}

/**
  * Special root node of the execution tree
  */
class ExecutionTreeRoot() extends ExecutionTree {
  var _children = List[Branch]()
  def children: List[ExecutionTree] = _children
  override def pathCondition(suffix: List[(AExpr, Boolean)] = Nil): List[(AExpr, Boolean)] = suffix
  override def parent: ExecutionTree = ??? // This should never happen
  def branch(cond: AExpr, symcond: AExpr, value: Boolean): ExecutionTree = {
    if (_children.isEmpty) {
      log.info(s"Encountered unseen branching condition: $cond")
      val node = new Branch(cond, symcond, this)
      _children = List(node)
    }
    val node = _children.last
    log.info(s"Exploring ${if (node.count(value) == 0) "unseen " else ""}$value branch")
    node.count(value) += 1
    node.branches(value)
  }
}

/**
  * A node representing a sub-tree of the execution tree with no observed branching
  */
class SubTreePlaceholder(val parent: Branch) extends ExecutionTree {
  override def children: List[ExecutionTree] = List()

  override def branch(cond: AExpr, symcond: AExpr, value: Boolean): ExecutionTree = {
    log.info(s"Encountered unseen branching condition: $cond")
    val node = new Branch(cond, symcond, parent)
    if (parent.branches(true) == this) {
      parent.branches(true) = node
    } else if (parent.branches(false) == this) {
      parent.branches(false) = node
    }
    log.info(s"Exploring unseen $value branch")
    node.count(value) += 1
    node.branches(value)
  }
}

/**
  * A node representing an unvisited sub-tree of the execution tree, which is known to be unsatisfiable
  */
class UnsatSubTree(val parent: Branch) extends ExecutionTree {
  override def children: List[ExecutionTree] = List()
  override def branch(cond: AExpr, symcond: AExpr, value: Boolean): ExecutionTree =
    ??? //this should never happen
}

/**
  * A node representing a branching in the execution tree
  */
class Branch(
  val condition: AExpr,
  val symcondition: AExpr,
  val parent: ExecutionTree,
  val branches: mutable.Map[Boolean, ExecutionTree] = mutable.Map(),
  val count: mutable.Map[Boolean, Int] = mutable.Map()
) extends ExecutionTree {

  branches(true) = new SubTreePlaceholder(this)
  branches(false) = new SubTreePlaceholder(this)
  count(true) = 0
  count(false) = 0

  def children: List[ExecutionTree] = branches.values.toList

  override def branch(cond: AExpr, symcond: AExpr, value: Boolean): ExecutionTree = {
    assert(cond == condition)
    assert(symcondition == symcond)
    log.info(s"Encountered seen branching condition: $cond")
    log.info(s"Exploring ${if (count(value) == 0) "unseen " else ""}$value branch")
    count(value) += 1
    branches(value)
  }

  def unsat(branch: Boolean): ExecutionTree =
    branches(branch) match {
      case _: SubTreePlaceholder =>
        branches(branch) = new UnsatSubTree(this)
        branches(branch)
      case _ =>
        ??? // Impossible: previously satisfiable branch becomes unsatisfiable
    }
}

object ExecutionTreePrinter {
  private def printNodeValue(treeNode: ExecutionTree, out: StringBuffer): Unit = {
    val str = treeNode match {
      case n: SubTreePlaceholder =>
        if (n.parent.branches(true) == treeNode && n.parent.count(true) > 0
          || n.parent.branches(false) == treeNode && n.parent.count(false) > 0)
          "<visited>"
        else
          "<??>"
      case _: UnsatSubTree => "<unsat>"
      case b: Branch => s"${b.symcondition} (${b.condition})"
      case _ => ???
    }
    out.append(str)
    out.append('\n')
  }

  private def printTree(treeNode: ExecutionTree, out: StringBuffer, isTrue: Boolean = true, indent: String = "", root: Boolean = false): Unit = {
    treeNode match {
      case b: Branch =>
        printTree(b.branches(true), out, true, indent + (if (isTrue) "          " else " |        "))
      case _ =>
    }

    out.append(indent)
    if (!root) {
      if (isTrue) {
        out.append(" /")
      } else {
        out.append(" \\")
      }
      out.append(s"----${if (isTrue) "T" else "F"}--- ")
    }

    printNodeValue(treeNode, out)

    treeNode match {
      case b: Branch =>
        printTree(b.branches(false), out, false, indent + (if (isTrue && !root) " |        " else "          "))
      case _ =>
    }
  }

  def printExecutionTree(treeNode: ExecutionTreeRoot): String = {
    val sb = new StringBuffer()
    printTree(treeNode.children.last, sb, root = true)
    sb.toString
  }
}
