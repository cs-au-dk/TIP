package tip.graph

import tip.dot.{ DotGraph, DotArrow, DotDirArrow, DotNode }
import tip.newAST._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import IntraControlFlowGraph._

object GNode {
  var _uid = -1

  def uid = {
    _uid += 1; _uid
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

object IntraControlFlowGraph {

  /**
   * A function assigning to each cfg node n an appropriate state w.r.t. the slides.
   */
  def prettyLabeller(f:AFunDeclaration)(n:ASTNode): String = {
    n match {
      case w:AWhileStmt => w.guard.toString
      case i:AIfStmt => i.guard.toString
      case _ => n.toString
    }
  }

  /**
   * The unit CFG w.r.t. the sequentialization,
   * i.e. collapse(x ~ seqUnit) = x
   */
  def seqUnit[A](): IntraControlFlowGraph[A] = {
    val f = AuxNode[A]()
    IntraControlFlowGraph(f, f)
  }

  /**
   * Implicit function to cast any node into a
   * single-node CFG
   */
  implicit def nodeToGraph[A](node: GNode[A]): IntraControlFlowGraph[A] = {
    IntraControlFlowGraph(node, node)
  }

  /**
   *  Generate a CFG from a program
   */
  def generateFromProgram[A](node: AProgram, init: AFunDeclaration => ASTNode => A): Map[AFunDeclaration, IntraControlFlowGraph[A]] = {
    node.fun.foldLeft(Map[AFunDeclaration, IntraControlFlowGraph[A]]()) { (m, fun) =>
      m + (fun -> generateFromAst(fun.stmts, init(fun)).collapse())
    }
  }

  /**
   * Generate a CFG from the body of a function
   */
  def generateFromAst[A](node: ASTNode, init: (ASTNode => A)): IntraControlFlowGraph[A] = {
    node match {
      case ass: AAssignStmt =>
        GRealNode(data = init(ass))
      case block: ABlockStmt =>
        block.content.foldLeft(IntraControlFlowGraph.seqUnit[A]) { (g, stmt) =>
          g ~ generateFromAst(stmt, init)
        }
      case iff: AIfStmt =>
        val ifBranch = GRealNode(data = init(iff))
        val trueG = generateFromAst(iff.ifBranch, init)
        val ifMerge: IntraControlFlowGraph[A] = AuxNode[A]()
        val falseG = iff.elseBranch.map { s => generateFromAst(s, init) }.getOrElse(ifMerge)
        ifBranch ~ trueG
        ifBranch ~ falseG
        trueG ~ ifMerge
        if (falseG != ifMerge) falseG ~ ifMerge
        IntraControlFlowGraph(ifBranch, ifMerge.exit)
      case out: AoutputStmt =>
        GRealNode(data = init(node))
      case ret: AReturnStmt =>
        GRealNode(data = init(node))
      case varr: AVarStmt =>
        GRealNode(data = init(node))
      case whl: AWhileStmt =>
        val whileG = GRealNode(data = init(node))
        val bodyG = generateFromAst(whl.innerBlock, init)
        val fake = AuxNode[A]()
        whileG ~ bodyG
        whileG ~ fake
        bodyG ~ whileG
        IntraControlFlowGraph(whileG, fake)
      case exp: AExpr => ???
      case fun: AFunDeclaration => ???
      case prog: AProgram => ???
    }
  }
}

/**
 * Intra-procedural control flow graph
 */
case class IntraControlFlowGraph[A](entry: GNode[A], exit: GNode[A]) {

  /**
   * Concatenate the left CFG with the right one, and returns the
   * concatenated one
   */
  def ~(rightG: IntraControlFlowGraph[A]): IntraControlFlowGraph[A] = {
    val pre = this.exit
    val post = rightG.entry

    pre.succ += post
    post.pred += pre

    IntraControlFlowGraph(this.entry, rightG.exit)
  }

  /**
   * Returns the set of nodes in the CFG
   */
  def nodes: Set[GNode[A]] = {
    val visited = mutable.Set[GNode[A]]()
    nodes_r(entry, visited)
    visited.toSet
  }

  /**
   * Remove all the auxiliary node from the CFG,
   * when it is possible
   */
  def collapse(): IntraControlFlowGraph[A] = {
    val newEntry = nodes.foldLeft(this.entry) { (curEntry, n1) =>
      n1 match {
        case aux: AuxNode[A] =>
          if (aux != curEntry || aux.succ.size == 1) {
            aux.pred.foreach { p => p.succ ++= aux.succ; p.succ -= aux }
            aux.succ.foreach { s => s.pred ++= aux.pred }
          }
          if (aux == curEntry)
            aux.succ.head
          else
            curEntry
        case _ =>
          curEntry
      }
    }
    IntraControlFlowGraph(newEntry, this.exit)
  }

  private def nodes_r(n: GNode[A], visited: mutable.Set[GNode[A]]): Unit = {
    if (!visited.contains(n)) {
      visited += n
      n.succ.foreach { n => nodes_r(n, visited) }
    }
  }

  /**
   * Returns a dot representation of the CFG
   */
  def toDot(): String = {
    val dotNodes = mutable.Map[GNode[A], DotNode]()
    val dotArrows = mutable.MutableList[DotArrow]()
    nodes.foreach { n =>
      dotNodes += (n -> new DotNode(n.toString))
    }
    nodes.foreach { n =>
      n.succ.foreach { dest =>
        dotArrows += new DotDirArrow(dotNodes(n), dotNodes(dest))
      }
    }
    var allNodes = dotNodes.values.seq.toList
    val entry = new DotNode("", Map("shape" -> "none"))
    val exit = new DotNode("", Map("shape" -> "none"))
    dotArrows += new DotDirArrow(entry, dotNodes(this.entry))
    dotArrows += new DotDirArrow(dotNodes(this.exit), exit)
    (new DotGraph("IntraproceduralControlFlowGraph",  entry :: exit :: allNodes, dotArrows)).toDotString
  }
}
