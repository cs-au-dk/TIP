package tip.cfg

import tip.analysis.ControlFlowAnalysis
import tip.ast.AstNodeData._
import tip.ast._
import tip.util.MapUtils._
import tip.util.TipProgramException

object InterproceduralProgramCfg {

  /**
    * Converts the given CFG node into a [[tip.cfg.FragmentCfg]].
    * Builds call and after-call nodes, and checks that the program is properly normalized.
    */
  private def callreturnNodeBuilder(n: CfgNode)(implicit declData: DeclarationData): FragmentCfg = {
    import AstOps._
    n match {
      case fentry: CfgFunEntryNode =>
        FragmentCfg.nodeToGraph(CfgFunEntryNode(data = fentry.data))
      case fentry: CfgFunExitNode =>
        FragmentCfg.nodeToGraph(CfgFunExitNode(data = fentry.data))
      case normal: CfgStmtNode =>
        normal.data match {
          case w: AWhileStmt =>
            assert(!w.guard.containsInvocation)
            FragmentCfg.nodeToGraph(CfgStmtNode(data = w.guard))
          case i: AIfStmt =>
            assert(!i.guard.containsInvocation)
            FragmentCfg.nodeToGraph(CfgStmtNode(data = i.guard))
          case as: AAssignStmt =>
            as.right match {
              case call: ACallFuncExpr =>
                assert(
                  !call.targetFun.containsInvocation
                    && call.args.forall(!_.containsInvocation)
                )
                val cnode = CfgCallNode(data = as)
                val retnode = CfgAfterCallNode(data = as)
                FragmentCfg.nodeToGraph(cnode) ~ FragmentCfg.nodeToGraph(retnode)
              case _ =>
                assert(!as.containsInvocation)
                FragmentCfg.nodeToGraph(CfgStmtNode(data = as))
            }
          case _ =>
            assert(!normal.data.containsInvocation)
            FragmentCfg.nodeToGraph(CfgStmtNode(data = normal.data))
        }
    }
  }

  private def usingFunctionDeclarationCallInfo()(implicit declData: DeclarationData): AAssignStmt => Set[AFunDeclaration] = { s: AAssignStmt =>
    s.right match {
      case ACallFuncExpr(target: AIdentifier, _, loc) =>
        declData(target) match {
          case d: AFunDeclaration => Set(d)
          case _ => new NoFunctionPointers().LanguageRestrictionViolation(s"$target is not a function identifier", loc)
        }
      case ACallFuncExpr(target, _, loc) => new NoFunctionPointers().LanguageRestrictionViolation(s"Indirect call to $target not supported", loc)
      case _ => Set[AFunDeclaration]()
    }
  }

  /**
    * Generates an interprocedural CFG from a program with [[tip.ast.NormalizedCalls]] and [[tip.ast.NoFunctionPointers]].
    */
  def generateFromProgram(prog: AProgram)(implicit declData: DeclarationData): InterproceduralProgramCfg = {
    val funGraphs = FragmentCfg.generateFromProgram(prog, callreturnNodeBuilder)
    val allEntries = funGraphs.mapValues(cfg => { assert(cfg.graphEntries.size == 1); cfg.graphEntries.head.asInstanceOf[CfgFunEntryNode] })
    val allExits = funGraphs.mapValues(cfg => { assert(cfg.graphExits.size == 1); cfg.graphExits.head.asInstanceOf[CfgFunExitNode] })

    // ensure that there are no function pointers or indirect calls
    new NormalizedCalls().assertContainsProgram(prog)
    new NoFunctionPointers().assertContainsProgram(prog)

    val callInfo = usingFunctionDeclarationCallInfo()

    new InterproceduralProgramCfg(allEntries, allExits, prog, callInfo)
  }

  /**
    * Generates an interprocedural CFG from a program with [[tip.ast.NormalizedCalls]], using [[tip.analysis.ControlFlowAnalysis]] for resolving function calls.
    */
  def generateFromProgramWithCfa(prog: AProgram)(implicit declData: DeclarationData): InterproceduralProgramCfg = {
    val funGraphs = FragmentCfg.generateFromProgram(prog, callreturnNodeBuilder)
    val allEntries = funGraphs.mapValues(cfg => { assert(cfg.graphEntries.size == 1); cfg.graphEntries.head.asInstanceOf[CfgFunEntryNode] })
    val allExits = funGraphs.mapValues(cfg => { assert(cfg.graphExits.size == 1); cfg.graphExits.head.asInstanceOf[CfgFunExitNode] })

    val cfaSolution = new ControlFlowAnalysis(prog).analyze()
    var callInfo: Map[AAssignStmt, Set[AFunDeclaration]] = Map()

    // Using result of CFA to build callInfo
    new DepthFirstAstVisitor[Unit] {
      override def visit(node: AstNode, arg: Unit): Unit =
        node match {
          case a @ AAssignStmt(_, ACallFuncExpr(id: AIdentifier, _, _), _) =>
            callInfo += a -> cfaSolution(id.declaration)
          case _ => visitChildren(node, arg)
        }
    }.visit(prog, ())

    new InterproceduralProgramCfg(allEntries, allExits, prog, callInfo)
  }
}

/**
  * Interprocedural control-flow graph for a program, where function calls are represented using call/after-call nodes.
  * Requires the program to be normalized using [[tip.ast.NormalizedCalls]], i.e. all calls are of the form x = f(..).
  *
  * @param funEntries map from AST function declarations to corresponding CFG function entry nodes
  * @param funExits map from AST function declarations to corresponding CFG function exit nodes
  * @param program the AST of the program
  * @param declData the declaration data
  * @param callInfo call graph
  */
class InterproceduralProgramCfg(
  funEntries: Map[AFunDeclaration, CfgFunEntryNode],
  funExits: Map[AFunDeclaration, CfgFunExitNode],
  val program: AProgram,
  val callInfo: AAssignStmt => Set[AFunDeclaration]
)(implicit declData: DeclarationData)
    extends ProgramCfg(program, funEntries, funExits) { graph =>

  /**
    * The node corresponding to entry of the main function.
    */
  def programEntry: CfgFunEntryNode = funEntries(program.mainFunction)

  /**
    * Map from [[tip.cfg.CfgFunEntryNode]] to the set of [[tip.cfg.CfgCallNode]]s calling the function.
    */
  var callers = Map[CfgFunEntryNode, Set[CfgCallNode]]().withDefaultValue(Set[CfgCallNode]())

  /**
    * Map from [[tip.cfg.CfgCallNode]] to the set of [[tip.cfg.CfgFunEntryNode]] of the called functions.
    */
  var callees = Map[CfgCallNode, Set[CfgFunEntryNode]]().withDefaultValue(Set())

  /**
    * Map from [[tip.cfg.CfgAfterCallNode]] to the set of [[tip.cfg.CfgFunExitNode]] of the called functions.
    */
  var calleeExits = Map[CfgAfterCallNode, Set[CfgFunExitNode]]().withDefaultValue(Set())

  /**
    * Map from [[tip.cfg.CfgFunExitNode]] to the set of [[tip.cfg.CfgAfterCallNode]]s of the calling functions.
    */
  var callerAfterCalls = Map[CfgFunExitNode, Set[CfgAfterCallNode]]().withDefaultValue(Set[CfgAfterCallNode]())

  /**
    * Map from [[tip.cfg.CfgFunEntryNode]] to the set of [[tip.cfg.CfgCallNode]]s in the function.
    */
  var callsInFunction: Map[CfgFunEntryNode, Set[CfgCallNode]] = _

  /**
    * Map from [[tip.cfg.CfgNode]] to the enclosing function entry node.
    */
  var enclosingFunctionEntry = Map[CfgNode, CfgFunEntryNode]()

  private def initdeps(): Unit = {
    nodes.foreach {
      case callNode: CfgCallNode =>
        val invoked = callInfo(callNode.data)
        val entries = invoked.map(d => funEntries(d))
        for (entry <- entries) {
          callers = callers + (entry -> (callers(entry) + callNode))
          callees = callees + (callNode -> (callees(callNode) + entry))
        }
      case afterNode: CfgAfterCallNode =>
        val invoked = callInfo(afterNode.data)
        val exits = invoked.map(d => funExits(d))
        for (exit <- exits) {
          callerAfterCalls = callerAfterCalls + (exit -> (callerAfterCalls(exit) + afterNode))
          calleeExits = calleeExits + (afterNode -> (calleeExits(afterNode) + exit))
        }
      case _ =>
    }
    enclosingFunctionEntry = functionNodes.reverse.mapValues(_.head)
    callsInFunction = entryToCalls
  }

  /**
    * Maps a function to the set of its nodes.
    */
  private def functionNodes: Map[CfgFunEntryNode, Set[CfgNode]] =
    funEntries.values.map { entry =>
      entry -> nodesRec(entry).toSet
    }.toMap

  /**
    * Maps each function (represented by it's entry node) to the set of call nodes it contains.
    */
  private def entryToCalls: Map[CfgFunEntryNode, Set[CfgCallNode]] =
    funEntries.values.map { entry =>
      (entry, nodesRec(entry).toSet.flatMap { n: CfgNode =>
        n match {
          case call: CfgCallNode => Some(call)
          case _ => None
        }
      })
    }.toMap

  initdeps()

  /**
    * An implicit class with convenience methods for CFG entry node operations that involve the whole-program CFG.
    */
  implicit class IpNodeInfoEntry(nd: CfgFunEntryNode) {

    /**
      * Returns the set of [[tip.cfg.CfgCallNode]]s of the called functions.
      */
    def callers: Set[CfgCallNode] =
      graph.callers(nd)

    /**
      * Returns the exit node of the function associated with this entry node
      */
    def exit: CfgFunExitNode = funExits(nd.data)
  }

  /**
    * An implicit class with convenience methods for CFG call node operations that involve the whole-program CFG.
    */
  implicit class IpNodeInfoCall(nd: CfgCallNode) {

    /**
      * Returns the set of [[tip.cfg.CfgFunEntryNode]] of the called functions.
      */
    def callees: Set[CfgFunEntryNode] =
      graph.callees(nd)

    /**
      * Returns the after-call node of this call node.
      */
    def afterCallNode: CfgAfterCallNode =
      nd.succ.head.asInstanceOf[CfgAfterCallNode]
  }

  /**
    * An implicit class with convenience methods for CFG after call node operations that involve the whole-program CFG.
    */
  implicit class IpNodeInfoAfterCall(nd: CfgAfterCallNode) {

    /**
      * Returns the [[tip.cfg.CfgFunExitNode]] of the called function.
      */
    def calledExit: Set[CfgFunExitNode] =
      graph.calleeExits(nd)

    /**
      * Returns the call node of this after call node
      */
    def callNode: CfgCallNode =
      nd.pred.head.asInstanceOf[CfgCallNode]
  }

  /**
    * An implicit class with convenience methods for CFG exit node operations that involve the whole-program CFG.
    */
  implicit class IpNodeInfoExit(nd: CfgFunExitNode) {

    /**
      * Map from [[tip.cfg.CfgFunExitNode]] to the set of [[tip.cfg.CfgAfterCallNode]]s of the calling function.
      */
    def callersAfterCall: Set[CfgAfterCallNode] =
      graph.callerAfterCalls(nd)

    /**
      * Returns the entry node of the function associated with this exit node
      */
    def entry: CfgFunEntryNode = funEntries(nd.data)
  }

  implicit class CallNodeContainsAssigment(nd: CfgCallNode) {

    def targetIdentifier: AIdentifier =
      nd.data match {
        case AAssignStmt(id: AIdentifier, _, _) => id
        case _ => throw new TipProgramException("Expected left-hand-side of call assignment to be an identifier")
      }

    def assignment: AAssignStmt =
      nd.data match {
        case as: AAssignStmt => as
      }

    def invocation: ACallFuncExpr =
      this.assignment.right match {
        case call: ACallFuncExpr => call
        case _ => throw new TipProgramException("Expected right-hand-side of call assignment to be a call")
      }
  }

  implicit class AfterCallNodeContainsAssigment(nd: CfgAfterCallNode) {

    def targetIdentifier: AIdentifier =
      nd.data match {
        case AAssignStmt(id: AIdentifier, _, _) => id
        case _ => throw new TipProgramException("Expected left-hand-side of call assignment to be an identifier")
      }

    def assignment: AAssignStmt =
      nd.data match {
        case as: AAssignStmt => as
      }

    def invocation: ACallFuncExpr =
      this.assignment.right match {
        case call: ACallFuncExpr => call
        case _ => throw new TipProgramException("Expected right-hand-side of call assignment to be a call")
      }
  }
}
