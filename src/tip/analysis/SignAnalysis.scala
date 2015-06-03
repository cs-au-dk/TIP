package tip.analysis

import tip.ast
import tip.ast._
import tip.lattices._
import tip.graph._
import tip.logging.Log
import tip.solvers._
import tip.graph.NodeOps._
import tip.MapUtils

object SignAnalysisCommons {
  val returnId = AIdentifier(s"#result", Loc(0, 0))()
}

/**
 * Transfer functions for sign analysis (intraprocedural only).
 */
trait IntraprocSignAnalysisTransferFunctions {

  val log = Log.typeLogger[this.type]()

  type Sign = SignLattice.SignElement.Value

  import SignLattice.SignElement._

  val domain: Set[GNode[AstNode]]

  val declaredVars = domain.map {_.declaredIdsIncludingParams}.flatten

  val statelattice = new MapLattice(declaredVars, SignLattice)

  /**
   * The transfer functions.
   */
  def transfer(n: GNode[AstNode], s: statelattice.Element): statelattice.Element = {
    NoPointer.checkCfgNodeLanguageRestriction(n)
    import NoPointer._
    n match {
      case r: GRealNode[AstNode] =>
        r.data match {
          //<---- Complete here
          // all others: like no-ops
          case _ => s
        }
      case _ => s
    }
  }

  private def unsupportedError(x: Any) = throw new IllegalArgumentException(s"Sign analysis meant to be run on programs without $x")
}

/**
 * Variant of [[tip.analysis.ForwardDependencies]] for interprocedural analysis.
 */
trait InterproceduralDependencies extends Dependencies[GNode[AstNode]] {

  val depLog = Log.typeLogger[this.type]()
  
  val cfg: ProgramCFG

  def called(n: GNode[AstNode]): Set[GNode[AstNode]]
  
  def callers(n: GNode[AstNode]): Set[GNode[AstNode]]

  /**
   * Like [[tip.analysis.ForwardDependencies.outdep()]] but with call and return edges.
   * A call node has an outdep to its after-call node.
   */
  override def outdep(n: GNode[AstNode]) = {
    val intraDep = n match {
      case call: CallNode[AstNode] => called(call)
      case exit: FunExit[AstNode] => callers(exit)
      case _ => Set()
    } 
    intraDep ++ n.succ.toSet
  }

  /**
   * Like [[tip.analysis.ForwardDependencies.indep()]] but returning an empty set for after-call nodes.
   */
  override def indep(n: GNode[AstNode]) = {
    n match {
      case acall: AfterCallNode[AstNode] => Set()
      case _ => n.pred.toSet
    }
  }
}

/**
 * Common functionality for interprocedural analysis.
 */
trait InterprocSignAnalysisMisc {
  
  val lattice: MapLattice[GNode[AstNode], LiftLattice[MapLattice[AIdentifier, SignLattice.type]]]

  val cfg: ProgramCFG
  
  NormalisedCallsLanguageRestrictions.checkCfgLanguageRestriction(cfg)
  NoPointer.checkCfgLanguageRestriction(cfg)

  def evalArgs(formalParams: Seq[AIdentifier], actualParams: Seq[AExpr], state: lattice.sublattice.sublattice.Element): lattice.sublattice.sublattice.Element = {
    formalParams.zip(actualParams).foldLeft(lattice.sublattice.sublattice.bottom) { case (acc, (id, exp)) =>
      acc + (id -> lattice.sublattice.sublattice.sublattice.eval(exp, state))
    }
  }
}

/**
 * Update function for sign analysis (including interprocedural).
 * This version is for the basic worklist algorithm.
 */
trait InterprocSignAnalysisFunctions extends MapLiftLatticeUpdateFunction[GNode[AstNode]] with InterprocSignAnalysisMisc {

  private val log = Log.typeLogger[this.type]()
  
  def called(n: GNode[AstNode]): Set[GNode[AstNode]]
  
  def callers(n: GNode[AstNode]): Set[GNode[AstNode]]
  
  override def funsub(n: GNode[AstNode], s: lattice.sublattice.Element, o: lattice.Element): lattice.sublattice.Element = {
    import NormalisedCallsLanguageRestrictions._
    import lattice.sublattice._

    log.debug(s"Processing $n ${n.getClass}")
    
    n match {
      
      // call nodes (like no-ops here)
      case call: CallNode[AstNode] => join(n, s, o)
      
      // function entry nodes
      case funentry: FunEntry[AstNode] => ???//<---- Complete here
      
      // function exit nodes (like no-ops here)
      case funexit: FunExit[AstNode] => join(n, s, o)
      
      // after-call nodes
      case aftercall: AfterCallNode[AstNode] => ???//<---- Complete here
      
      case GRealNode(_, _,_ , ret: AReturnStmt) => {
        val j = join(n, s, o)
        j + (SignAnalysisCommons.returnId -> lattice.sublattice.sublattice.sublattice.eval(ret.value, j))
      }
      
      case _ => super.funsub(n, s, o)
    }
  }
}

/**
 * Update functions for sign analysis (including interprocedural), propagation style.
 * This is a variant of [[InterprocSignAnalysisFunctions]] for use with [[tip.solvers.WorklistFixpointPropagationSolver]].
 */
trait InterprocSignAnalysisFunctionsWithPropagation extends MapLiftLatticeUpdateFunction[GNode[AstNode]]
  with WorklistFixpointPropagationSolver[GNode[AstNode]] with InterprocSignAnalysisMisc {
  
  override def funsub(n: GNode[AstNode], s: lattice.sublattice.Element, o: lattice.Element): lattice.sublattice.Element = {
    import cfg._
    import NormalisedCallsLanguageRestrictions._
    import lattice.sublattice._
    
    // helper function that propagates dataflow from a function exit node to an after-call node 
    def returnflow(funexit: FunExit[AstNode], aftercall: AfterCallNode[AstNode]) {
      val id = aftercall.assignment.leftId
      propagate(o(aftercall.pred.head) + (id.meta.definition.get.asInstanceOf[AIdentifier] -> s(SignAnalysisCommons.returnId)), aftercall)
    }

    n match {

      // call nodes
      case call: CallNode[AstNode] => {
        val calledFun = call.called.get.data.asInstanceOf[AFunDeclaration]
        propagate(evalArgs(calledFun.args, call.invocation.args, s), call.called.head)
        returnflow(call.succ.head.calledExits.head, call.succ.head.asInstanceOf[AfterCallNode[AstNode]]) // make sure existing return flow gets propagated
        lattice.sublattice.bottom // no flow directly to the after-call node
      }

      // function entry nodes (like no-op here)
      case funentry: FunEntry[AstNode] => s
      
      // function exit nodes
      case funexit: FunExit[AstNode] => {
        for (aftercall <- funexit.callersAfterCall) returnflow(funexit, aftercall)
        lattice.sublattice.bottom // no successors for this kind of node, but we have to return something
      }
      
      // after-call nodes (like no-op here)
      case aftercall: AfterCallNode[AstNode] => s

      case GRealNode(_, _,_ , ret: AReturnStmt) => {
        s + (SignAnalysisCommons.returnId -> lattice.sublattice.sublattice.sublattice.eval(ret.value, s))
      }

      case _ => super.funsub(n, s, o)
    }
  }
}

/**
 * Base class for sign analysis.
 */
abstract class IntraprocSignAnalysis(cfg: ControlFlowGraph[AstNode]) extends FlowSensitiveAnalysis(cfg)
  with IntraprocSignAnalysisTransferFunctions with MapLiftLatticeUpdateFunction[GNode[AstNode]] with ForwardDependencies {

  val lattice = new MapLattice(cfg.nodes, new LiftLattice(statelattice))
}

/**
 * Intraprocedural sign analysis that uses [[tip.solvers.SimpleFixpointSolver]].
 */
class IntraprocSignAnalysisSimpleSolver(cfg: ControlFlowGraph[AstNode])
  extends IntraprocSignAnalysis(cfg) with SimpleFixpointSolver with MapLatticeUpdateFunction[GNode[AstNode]]

/**
 * Intraprocedural sign analysis that uses [[tip.solvers.WorklistFixpointSolver]].
 */
class IntraprocSignAnalysisWorklistSolver(cfg: ControlFlowGraph[AstNode])
  extends IntraprocSignAnalysis(cfg) with WorklistFixpointSolver[GNode[AstNode]]

/**
 * Intraprocedural sign analysis that uses [[tip.solvers.WorklistFixpointSolverWithInit]],
 * with all function entries as start nodes.
 */
class IntraprocSignAnalysisWorklistSolverWithInit(cfg: ControlFlowGraph[AstNode])
  extends IntraprocSignAnalysisWorklistSolver(cfg) with WorklistFixpointSolverWithInit[GNode[AstNode]] {

  val first = cfg.entries.values.toSet
}

/**
 * Intraprocedural sign analysis that uses [[tip.solvers.WorklistFixpointSolverWithInit]]
 * together with [[tip.solvers.WorklistFixpointPropagationSolver]].
 */
class IntraprocSignAnalysisWorklistSolverWithInitAndPropagation(cfg: ControlFlowGraph[AstNode])
  extends IntraprocSignAnalysisWorklistSolverWithInit(cfg) with WorklistFixpointPropagationSolver[GNode[AstNode]] {
  
  override val init = lattice.sublattice.Lift(lattice.sublattice.sublattice.bottom)
}

/**
 * Interprocedural sign analysis that uses [[tip.solvers.WorklistFixpointSolverWithInit]].
 */
class InterprocSignAnalysisWorklistSolverWithInit(val cfg: ProgramCFG)
  extends IntraprocSignAnalysisWorklistSolverWithInit(cfg) with InterprocSignAnalysisFunctions
  with InterproceduralDependencies {

  override val first = Set(cfg.programEntry)
 
  import cfg._
  
  override def called(n: GNode[AstNode]): Set[GNode[AstNode]] = n.called.map(Set(_)).getOrElse(Set()) ++ n.calledExits.map(Set(_)).getOrElse(Set())  
  
  override def callers(n: GNode[AstNode]): Set[GNode[AstNode]] = n.callers ++ n.callersAfterCall
}
  
/**
 * Interprocedural sign analysis that uses [[tip.solvers.WorklistFixpointSolverWithInit]]
 * together with [[tip.solvers.WorklistFixpointPropagationSolver]].
 * Note that this class uses [[tip.analysis.ForwardDependencies]] which has no interprocedural outdeps,
 * and it does not use indeps.
 */
class InterprocSignAnalysisWorklistSolverWithInitAndPropagation(val cfg: ProgramCFG)
  extends IntraprocSignAnalysisWorklistSolverWithInitAndPropagation(cfg) 
  with InterprocSignAnalysisFunctionsWithPropagation with ForwardDependencies {

  override val first = Set(cfg.programEntry)
}

/**
 * Interprocedural sign analysis that uses [[tip.solvers.WorklistFixpointSolverWithInit]],
 * using [[ControlFlowAnalysis]] to resolve function calls.
 */
class InterprocSignAnalysisWorklistSolverWithInitAndCFA(val cfg: ProgramCFG)
  extends IntraprocSignAnalysisWorklistSolverWithInit(cfg) with InterprocSignAnalysisFunctions
  with InterproceduralDependencies {

  override val first = Set(cfg.programEntry)

  private val cfaLog = Log.typeLogger[this.type]()
  
  val cfaAnalysis = new ControlFlowAnalysis(cfg.program)

  private val cfaSolution = cfaAnalysis.solver.getSolution.map(p => p._1.n -> p._2.map { x => x.fun })
  
  import MapUtils._
  private val cfaSolutionRev = cfaSolution.reverse

  import NormalisedCallsLanguageRestrictions._
  
  def called(n: GNode[AstNode]) = {
    val res = n match {
      case call: CallNode[AstNode] =>  cfaSolution(call.invocation.targetFun.meta.definition.get).map { x => cfg.entries(x) }
      case acall: AfterCallNode[AstNode] =>
        cfaSolution(acall.invocation.targetFun.meta.definition.get).map { x => cfg.exits(x) }
      case _ => Set[GNode[AstNode]]() 
    }
    res
  }
  
  def callers(n: GNode[AstNode]) = {
    val res = n match {
      case fexit: FunExit[AstNode] =>
        val callNodes = cfg.nodes.collect{case call: AfterCallNode[AstNode] => call}
        val possibleCallSites = cfaSolutionRev(fexit.data.asInstanceOf[AFunDeclaration])
        callNodes.filter { x => possibleCallSites.contains(x.invocation.targetFun.meta.definition.get) }.map(x => x: GNode[AstNode])
      case fentry: FunEntry[AstNode] => 
        val callNodes = cfg.nodes.collect{case call: CallNode[AstNode] => call}
        val possibleCallSites = cfaSolutionRev(fentry.data.asInstanceOf[AFunDeclaration])
        callNodes.filter { x => possibleCallSites.contains(x.invocation.targetFun.meta.definition.get) }.map(x => x: GNode[AstNode])
      case _ => Set[GNode[AstNode]]() 
    }
    res
  }
}
