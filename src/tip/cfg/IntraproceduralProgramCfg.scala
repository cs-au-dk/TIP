package tip.cfg

import tip.ast.AstNodeData.DeclarationData
import tip.ast._

object IntraproceduralProgramCfg {

  /**
    * Converts the given CFG node into a [[tip.cfg.FragmentCfg]].
    * No call and after-call nodes are generated.
    */
  private def simpleNodeBuilder(n: CfgNode): FragmentCfg =
    n match {
      case fentry: CfgFunEntryNode =>
        FragmentCfg.nodeToGraph(CfgFunEntryNode(data = fentry.data))
      case fentry: CfgFunExitNode =>
        FragmentCfg.nodeToGraph(CfgFunExitNode(data = fentry.data))
      case normal: CfgStmtNode =>
        normal.data match {
          case w: AWhileStmt => FragmentCfg.nodeToGraph(CfgStmtNode(data = w.guard))
          case i: AIfStmt => FragmentCfg.nodeToGraph(CfgStmtNode(data = i.guard))
          case o => FragmentCfg.nodeToGraph(CfgStmtNode(data = o))
        }
    }

  /**
    * Generates an [[IntraproceduralProgramCfg]] from a program.
    */
  def generateFromProgram(prog: AProgram)(implicit declData: DeclarationData): IntraproceduralProgramCfg = {
    val funGraphs = FragmentCfg.generateFromProgram(prog, simpleNodeBuilder)
    val allEntries = funGraphs.mapValues(cfg => { assert(cfg.graphEntries.size == 1); cfg.graphEntries.head.asInstanceOf[CfgFunEntryNode] })
    val allExits = funGraphs.mapValues(cfg => { assert(cfg.graphExits.size == 1); cfg.graphExits.head.asInstanceOf[CfgFunExitNode] })
    new IntraproceduralProgramCfg(prog, allEntries, allExits)
  }
}

/**
  * Control-flow graph for a program, where function calls are represented as expressions, without using call/after-call nodes.
  */
class IntraproceduralProgramCfg(prog: AProgram, funEntries: Map[AFunDeclaration, CfgFunEntryNode], funExits: Map[AFunDeclaration, CfgFunExitNode])
    extends ProgramCfg(prog, funEntries, funExits)
