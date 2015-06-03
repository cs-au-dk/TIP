package tip.graph

import tip.ast._


object IntraproceduralCFG {

  /**
   * A function assigning to each cfg node n an appropriate graph w.r.t. the slides.
   * No call and after-call are generated
   */
  private def simpleNodeBuilder(f: AFunDeclaration)(n: GNode[AstNode]): ControlFlowGraph[AstNode] = {
    n match {
      case fentry: FunEntry[AstNode] =>
        ControlFlowGraph.nodeToGraph(FunEntry(data = fentry.data), f)
      case fentry: FunExit[AstNode] =>
        ControlFlowGraph.nodeToGraph(FunExit(data = fentry.data), f)
      case normal: GRealNode[AstNode] =>
        normal.data match {
          case w: AWhileStmt => ControlFlowGraph.nodeToGraph(GRealNode(data = w.guard), f)
          case i: AIfStmt => ControlFlowGraph.nodeToGraph(GRealNode(data = i.guard), f)
          case o => ControlFlowGraph.nodeToGraph(GRealNode(data = o), f)
        }
    }
  }

  /**
   * Generate an [[IntraproceduralCFG]] from a program
   */
  def generateFromProgram(node: AProgram): IntraproceduralCFG = {

    val cfg = ControlFlowGraph.generateFromAst(node, simpleNodeBuilder, null)
    cfg.collapse()

    new IntraproceduralCFG(cfg.entries, cfg.exits)
  }
}

class IntraproceduralCFG(
                          entries: Map[AFunDeclaration, GNode[AstNode]],
                          exits: Map[AFunDeclaration, GNode[AstNode]]
                          ) extends ControlFlowGraph[AstNode](entries, exits)