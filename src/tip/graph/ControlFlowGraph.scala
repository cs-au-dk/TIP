package tip.graph

import tip.ast._
import tip.dot.{DotArrow, DotDirArrow, DotGraph, DotNode}

import scala.collection.mutable
import scala.language.implicitConversions

object ControlFlowGraph {

  /**
   * The unit CFG w.r.t. the sequentialization,
   * i.e. collapse(x ~ seqUnit) = x
   */
  def seqUnit[A](fun: AFunDeclaration): ControlFlowGraph[A] = {
    val f = AuxNode[A]()
    new ControlFlowGraph(Map(fun -> f), Map(fun -> f))
  }

  /**
   * Function to cast any node into a
   * single-node CFG
   */
  def nodeToGraph[A](node: GNode[A], fun: AFunDeclaration): ControlFlowGraph[A] = {
    new ControlFlowGraph(Map(fun -> node), Map(fun -> node))
  }

  /**
   * Generate a CFG from the body of a function
   */
  def generateFromAst[A](node: AstNode, nodeBuilder: (AFunDeclaration => GNode[AstNode] => ControlFlowGraph[A]), fun: AFunDeclaration): ControlFlowGraph[A] = {
    node match {
      case prog: AProgram =>
        val allFun = prog.fun.map { f =>
          generateFromAst(f, nodeBuilder, f)
        }
        val allEntries = allFun.map(cfg => cfg.entries).foldLeft(Map[AFunDeclaration, GNode[A]]()) { (a, m) => a ++ m }
        val allExits = allFun.map(cfg => cfg.exits).foldLeft(Map[AFunDeclaration, GNode[A]]()) { (a, m) => a ++ m }
        new ControlFlowGraph(allEntries, allExits)
      case fun: AFunDeclaration =>
        val blk = generateFromAst(fun.stmts, nodeBuilder, fun)
        val entry = nodeBuilder(fun)(FunEntry(data = node))
        val exit = nodeBuilder(fun)(FunExit(data = node))
        (entry ~ blk) ~ exit
      case ass: AAssignStmt =>
        nodeBuilder(fun)(GRealNode(data = node))
      case block: ABlockStmt =>
        block.content.foldLeft(ControlFlowGraph.seqUnit[A](fun)) { (g, stmt) =>
          g ~ generateFromAst(stmt, nodeBuilder, fun)
        }
      case iff: AIfStmt =>
        val ifBranch = nodeBuilder(fun)(GRealNode(data = node))
        val trueG = generateFromAst(iff.ifBranch, nodeBuilder, fun)
        val ifMerge: ControlFlowGraph[A] = nodeToGraph(AuxNode[A](), fun)
        val falseG = iff.elseBranch.fold(ifMerge) { s => generateFromAst(s, nodeBuilder, fun) }
        ifBranch ~ trueG
        ifBranch ~ falseG
        trueG ~ ifMerge
        if (falseG != ifMerge) falseG ~ ifMerge
        new ControlFlowGraph(Map(fun -> ifBranch.entries(fun)), Map(fun -> ifMerge.exits(fun)))
      case out: AoutputStmt =>
        nodeBuilder(fun)(GRealNode(data = node))
      case ret: AReturnStmt =>
        nodeBuilder(fun)(GRealNode(data = node))
      case varr: AVarStmt =>
        nodeBuilder(fun)(GRealNode(data = node))
      case whl: AWhileStmt =>
        val whileG = nodeBuilder(fun)(GRealNode(data = node))
        val bodyG = generateFromAst(whl.innerBlock, nodeBuilder, fun)
        val fake = nodeToGraph(AuxNode[A](), fun)
        whileG ~ bodyG
        whileG ~ fake
        bodyG ~ whileG
        new ControlFlowGraph(Map(fun -> whileG.entries(fun)), Map(fun -> fake.exits(fun)))
      case exp: AExpr => ???
    }
  }
}

class ControlFlowGraph[A](
                           val entries: Map[AFunDeclaration, GNode[A]],
                           val exits: Map[AFunDeclaration, GNode[A]]
                           ) {

  /**
   * Concatenate the left CFG with the right one, and returns the
   * concatenated one
   */
  def ~(rightG: ControlFlowGraph[A]): ControlFlowGraph[A] = {

    assert(rightG.entries.size == 1 && this.entries.size == 1 && this.exits.keys.head == rightG.entries.keys.head)

    val fun = rightG.entries.keys.head

    val pre = exits(fun)
    val post = rightG.entries(fun)

    pre.succ += post
    post.pred += pre

    new ControlFlowGraph(Map(fun -> entries(fun)), Map(fun -> rightG.exits(fun)))
  }

  /**
   * Returns the set of nodes in the CFG
   */
  def nodes: Set[GNode[A]] = {
    val visited = mutable.Set[GNode[A]]()
    entries.values.foreach { entry => nodes_r(entry, visited) }
    visited.toSet
  }

  /**
   * Returns the set of nodes in the CFG for a given function
   */
  def funNodes(fun: AFunDeclaration): Set[GNode[A]] = {
    val visited = mutable.Set[GNode[A]]()
    nodes_r(entries(fun), visited)
    visited.toSet
  }


  private def nodes_r(n: GNode[A], visited: mutable.Set[GNode[A]]): Unit = {
    if (!visited.contains(n)) {
      visited += n
      n.succ.foreach { n => nodes_r(n, visited) }
    }
  }


  /**
   * Remove all the auxiliary node from the CFG,
   * when it is possible
   */
  def collapse(): Unit = {
    nodes.foreach {
      case aux: AuxNode[A] =>
        if (aux.succ.size == 1) {
          aux.pred.foreach { p => p.succ ++= aux.succ; p.succ -= aux }
          aux.succ.foreach { s => s.pred ++= aux.pred; s.pred -= aux }
        }
      case _ =>
    }
  }

  /**
   * Return a map associating each node with its rank.
   * The rank is defined such that
   * rank(x) < rank(y) iff y is visited after x in a depth-first
   * visit of the control flow graph
   */
  def rank: Map[GNode[A], Int] = {
    def rank_r(elems: List[GNode[A]], visited: List[List[GNode[A]]], level: Int): Map[GNode[A], Int] = {
      val curLevel = elems.map { x => x -> level }.toMap
      val newNeighbors = elems.flatMap(_.succ).filterNot(visited.flatten.contains).distinct
      if (newNeighbors.isEmpty)
        Map() ++ curLevel
      else
        rank_r(newNeighbors, newNeighbors :: visited, level + 1) ++ curLevel
    }
    rank_r(entries.values.toList, List(entries.values.toList), 0)
  }

  /**
   * Returns a dot representation of the CFG,
   * each node is labeled using the function labeler
   */
  def toDot(labeler: (GNode[A] => String), idGen: GNode[A] => String): String = {
    val dotNodes = mutable.Map[GNode[A], DotNode]()
    var dotArrows = mutable.MutableList[DotArrow]()

    nodes.foreach { n =>
      dotNodes += (n -> new DotNode(s"${idGen(n)}", labeler(n), Map()))
    }
    nodes.foreach { n =>
      n.succ.foreach { dest =>
        dotArrows += new DotDirArrow(dotNodes(n), dotNodes(dest))
      }
    }

    dotArrows = dotArrows.sortBy(arr => arr.fromNode.id + "-" + arr.toNode.id)
    var allNodes = dotNodes.values.seq.toList.sortBy(n => n.id)

    new DotGraph("CFG", allNodes, dotArrows).toDotString
  }
}
