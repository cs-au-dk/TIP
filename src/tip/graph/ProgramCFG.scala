package tip.graph

import tip.ast._


object ProgramCFG {

  /**
   * Check that the program is normalised while it builds an intraprocedural CFG
   * with call and return nodes for functions
   */
  def callreturnNodeBuilder(f: AFunDeclaration)(n: GNode[AstNode]): ControlFlowGraph[AstNode] = {
    import AstOps._
    n match {
      case fentry: FunEntry[AstNode] =>
        ControlFlowGraph.nodeToGraph(FunEntry(data = fentry.data), f)
      case fentry: FunExit[AstNode] =>
        ControlFlowGraph.nodeToGraph(FunExit(data = fentry.data), f)
      case normal: GRealNode[AstNode] =>
        normal.data match {
          case w: AWhileStmt =>
            assert(!w.containsInvocation)
            ControlFlowGraph.nodeToGraph[AstNode](GRealNode(data = w.guard), f)
          case i: AIfStmt =>
            assert(!i.containsInvocation)
            ControlFlowGraph.nodeToGraph[AstNode](GRealNode(data = i.guard), f)
          case ass: AAssignStmt =>
            ass.right match {
              case call: ACallFuncExpr =>
                assert(!call.targetFun.containsInvocation
                  && call.args.forall(!_.containsInvocation))
                val cnode = CallNode(data = normal.data)
                val retnode = AfterCallNode(data = normal.data)
                ControlFlowGraph.nodeToGraph[AstNode](cnode, f) ~ ControlFlowGraph.nodeToGraph[AstNode](retnode, f)
              case _ =>
                assert(!ass.containsInvocation)
                ControlFlowGraph.nodeToGraph[AstNode](GRealNode(data = ass), f)
            }
          case _ =>
            assert(!normal.data.containsInvocation)
            ControlFlowGraph.nodeToGraph[AstNode](GRealNode(data = normal.data), f)
        }
    }
  }


  /**
   * Generate an [[ProgramCFG]] from a program
   */
  def generateFromProgram(node: AProgram): ControlFlowGraph[AstNode] = {
    val cfg = ControlFlowGraph.generateFromAst(node, callreturnNodeBuilder, null)
    cfg.collapse()

    new ProgramCFG(cfg.entries, cfg.exits, node)
  }
}


class ProgramCFG(
                  entries: Map[AFunDeclaration, GNode[AstNode]],
                  exits: Map[AFunDeclaration, GNode[AstNode]],
                  val program: AProgram
                  ) extends ControlFlowGraph[AstNode](entries, exits) {
  graph =>


  /**
   * The node corresponding to the main function
   */
  def programEntry = entries.find(p => p._1 == program.mainFunction).get._2


  /**
   * Map from [[tip.graph.FunEntry]] to the set of [[tip.graph.CallNode]] calling the function
   * for all the other nodes, this map maps to the empty set                            
   */
  var callers = Map[GNode[AstNode], Set[CallNode[AstNode]]]().withDefaultValue(Set[CallNode[AstNode]]())

  /**
   * Map from [[tip.graph.CallNode]] to the[[tip.graph.FunEntry]] of the called function
   * for all the other nodes this map maps to None                            
   */
  var called = Map[GNode[AstNode], Option[FunEntry[AstNode]]]().withDefaultValue(None)

  /**
   * Map from the [[tip.graph.AfterCallNode]] to the [[tip.graph.FunExit]] of the called function
   * for all the other nodes this map maps to None
   */
  var calledExits = Map[GNode[AstNode], Option[FunExit[AstNode]]]().withDefaultValue(None)

  /**
   * Map from the [[tip.graph.FunExit]] to the set of [[tip.graph.AfterCallNode]] of the calling function
   * for all the other nodes this map maps to the empty set
   */
  var callersAfterCall = Map[GNode[AstNode], Set[AfterCallNode[AstNode]]]().withDefaultValue(Set[AfterCallNode[AstNode]]())

  NormalisedCallsLanguageRestrictions.checkCfgLanguageRestriction(this)

  import NormalisedCallsLanguageRestrictions._

  private def initdeps(): Unit = {
    nodes.foreach {
      case callNode: CallNode[AstNode] =>
        val inv = callNode.invocation
        inv.targetFun.meta.definition.get match {
          case funDecl: AFunDeclaration =>
            val funEntryNode = entries(funDecl).asInstanceOf[FunEntry[AstNode]]

            val newSet = callers(funEntryNode) + callNode
            callers = callers + (funEntryNode -> newSet)

            called = called + (callNode -> Some(funEntryNode))
          case _ =>
        }
      case aftercNode: AfterCallNode[AstNode] =>
        val inv = aftercNode.invocation
        inv.targetFun.meta.definition.get match {
          case funDecl: AFunDeclaration =>
            val funExitNode = exits(funDecl).asInstanceOf[FunExit[AstNode]]

            calledExits = calledExits + (aftercNode -> Some(funExitNode))

            val newSet = callersAfterCall(funExitNode) + aftercNode
            callersAfterCall = callersAfterCall + (funExitNode -> newSet)
          case _ =>
        }
      case _ =>
    }

  }

  initdeps()

  implicit class IpNodeInfo(nd: GNode[AstNode]) {

    def callers: Set[CallNode[AstNode]] = {
      graph.callers(nd)
    }

    def called: Option[FunEntry[AstNode]] = {
      graph.called(nd)
    }

    def calledExits: Option[FunExit[AstNode]] = {
      graph.calledExits(nd)
    }

    def callersAfterCall: Set[AfterCallNode[AstNode]] = {
      graph.callersAfterCall(nd)
    }
  }

}
