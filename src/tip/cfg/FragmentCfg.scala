package tip.cfg

import tip.ast.AstNodeData.DeclarationData
import tip.ast._
import tip.util._

import scala.collection.mutable

object FragmentCfg {

  /**
    * Generates a CFG for each function in the given program.
    */
  def generateFromProgram(prog: AProgram, nodeBuilder: CfgNode => FragmentCfg)(implicit declData: DeclarationData): Map[AFunDeclaration, FragmentCfg] =
    prog.funs.map { f =>
      f -> FragmentCfg.generateFromFunction(f, nodeBuilder)
    }.toMap

  /**
    * Constructs an empty CFG.
    */
  private def seqUnit(): FragmentCfg =
    new FragmentCfg(Set(), Set())

  /**
    * Converts a CFG node to a one-node CFG.
    */
  def nodeToGraph(node: CfgNode): FragmentCfg =
    new FragmentCfg(Set(node), Set(node))

  /**
    * Generates a CFG from the body of a function.
    */
  def generateFromFunction(fun: AFunDeclaration, nodeBuilder: CfgNode => FragmentCfg): FragmentCfg = {

    def recGen(node: AstNode): FragmentCfg =
      node match {
        case fun: AFunDeclaration =>
          val blk = recGen(fun.stmts)
          val entry = nodeBuilder(CfgFunEntryNode(data = fun))
          val exit = nodeBuilder(CfgFunExitNode(data = fun))
          entry ~ blk ~ exit
        case _: AAssignStmt =>
          nodeBuilder(CfgStmtNode(data = node))
        case block: ABlock =>
          block.body.foldLeft(seqUnit()) { (g, stmt) =>
            g ~ recGen(stmt)
          }
        case iff: AIfStmt =>
          val ifGuard = nodeBuilder(CfgStmtNode(data = node))
          val trueBranch = recGen(iff.ifBranch)
          val falseBranch = iff.elseBranch.map {
            recGen(_)
          }
          val guardedTrue = ifGuard ~ trueBranch
          val guardedFalse = falseBranch.map(fb => ifGuard ~ fb)
          guardedFalse.fold(guardedTrue | ifGuard)(guardedTrue | _)
        case _: AOutputStmt =>
          nodeBuilder(CfgStmtNode(data = node))
        case _: AReturnStmt =>
          nodeBuilder(CfgStmtNode(data = node))
        case _: AVarStmt =>
          nodeBuilder(CfgStmtNode(data = node))
        case whl: AWhileStmt =>
          val whileG = nodeBuilder(CfgStmtNode(data = node))
          val bodyG = recGen(whl.innerBlock)
          val loopingBody = whileG ~ bodyG ~ whileG
          loopingBody | whileG
        case _: AErrorStmt =>
          nodeBuilder(CfgStmtNode(data = node))
        case _: AExpr | _: AIdentifierDeclaration | _: AProgram => ???
      }

    recGen(fun)
  }
}

/**
  * Fragment of a control-flow graph.
  * Describes a fragment of a TIP program, for example one or more statements or function bodies.
  *
  * @param graphEntries set of entry nodes of the fragment
  * @param graphExits set of exit nodes of the fragment
  *
  * @see [[tip.cfg.InterproceduralProgramCfg]], [[tip.cfg.IntraproceduralProgramCfg]]
  */
class FragmentCfg(private[cfg] val graphEntries: Set[CfgNode], private[cfg] val graphExits: Set[CfgNode]) {

  /**
    * Returns true if this is the unit CFG w.r.t. to concatenation.
    */
  def isUnit: Boolean = graphEntries.isEmpty && graphExits.isEmpty

  /**
    * Returns the concatenation of this CFG with `after`.
    */
  def ~(after: FragmentCfg): FragmentCfg =
    if (isUnit)
      after
    else if (after.isUnit)
      this
    else {
      graphExits.foreach(_.succ ++= after.graphEntries)
      after.graphEntries.foreach(_.pred ++= graphExits)
      new FragmentCfg(graphEntries, after.graphExits)
    }

  /**
    * Returns the union of this CFG with `other`.
    */
  def |(other: FragmentCfg): FragmentCfg =
    new FragmentCfg(other.graphEntries.union(graphEntries), other.graphExits.union(graphExits))

  /**
    * Returns the set of nodes in the CFG.
    */
  def nodes: Set[CfgNode] =
    graphEntries.flatMap { entry =>
      nodesRec(entry).toSet
    }

  protected def nodesRec(n: CfgNode, visited: mutable.Set[CfgNode] = mutable.Set()): mutable.Set[CfgNode] = {
    if (!visited.contains(n)) {
      visited += n
      n.succ.foreach { n =>
        nodesRec(n, visited)
      }
    }
    visited
  }

  /**
    * Returns a map associating each node with its rank.
    * The rank is defined such that
    * rank(x) < rank(y) iff y is visited after x in a depth-first
    * visit of the control-flow graph
    */
  lazy val rank: Map[CfgNode, Int] = {
    def rankRec(elems: List[CfgNode], visited: List[List[CfgNode]], level: Int): Map[CfgNode, Int] = {
      val curLevel = elems.map { x =>
        x -> level
      }.toMap
      val newNeighbors = elems.flatMap(_.succ).filterNot(visited.flatten.contains).distinct
      if (newNeighbors.isEmpty)
        Map() ++ curLevel
      else
        rankRec(newNeighbors, newNeighbors :: visited, level + 1) ++ curLevel
    }
    rankRec(graphEntries.toList, List(graphEntries.toList), 0)
  }

  /**
    * Returns a Graphviz dot representation of the CFG.
    * Each node is labeled using the given function labeler.
    */
  def toDot(labeler: CfgNode => String, idGen: CfgNode => String): String = {
    val dotNodes = mutable.Map[CfgNode, DotNode]()
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
    val allNodes = dotNodes.values.seq.toList.sortBy(n => n.id)
    new DotGraph("CFG", allNodes, dotArrows).toDotString
  }
}

/**
  * Control-flow graph for an entire program.
  *
  * @param prog AST of the program
  * @param funEntries map from AST function declarations to CFG function entry nodes
  * @param funExits map from AST function declarations to CFG function exit nodes
  */
abstract class ProgramCfg(val prog: AProgram, val funEntries: Map[AFunDeclaration, CfgFunEntryNode], val funExits: Map[AFunDeclaration, CfgFunExitNode])
    extends FragmentCfg(funEntries.values.toSet, funExits.values.toSet)
