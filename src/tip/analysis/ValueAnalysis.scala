package tip.analysis

import tip.ast._
import tip.cfg.CfgOps._
import tip.cfg._
import tip.lattices._
import tip.solvers._
import tip.ast.AstNodeData.{AstNodeWithDeclaration, DeclarationData}

import scala.collection.immutable.Set

/**
  * General definitions for value analysis.
  */
trait ValueAnalysisMisc {

  implicit val declData: DeclarationData

  val cfg: ProgramCfg

  /**
    * The lattice of abstract values.
    */
  val valuelattice: LatticeWithOps

  /**
    * Set of declared variables, used by `statelattice`.
    */
  val declaredVars: Set[ADeclaration] = cfg.nodes.flatMap(_.declaredVarsAndParams)

  /**
    * The lattice of abstract states.
    */
  val statelattice: MapLattice[ADeclaration, valuelattice.type] = new MapLattice(valuelattice)

  /**
    * Default implementation of eval.
    */
  def eval(exp: AExpr, env: statelattice.Element)(implicit declData: DeclarationData): valuelattice.Element = {
    import valuelattice._
    exp match {
      case id: AIdentifier => env(id)
      case n: ANumber => num(n.value)
      case bin: ABinaryOp =>
        val left = eval(bin.left, env)
        val right = eval(bin.right, env)
        bin.operator match {
          case Eqq => eqq(left, right)
          case GreatThan => gt(left, right)
          case Divide => div(left, right)
          case Minus => minus(left, right)
          case Plus => plus(left, right)
          case Times => times(left, right)
          case _ => ???
        }
      case _: AInput => valuelattice.top
      case _ => ???
    }
  }

  /**
    * Transfer function for state lattice elements.
    */
  def localTransfer(n: CfgNode, s: statelattice.Element): statelattice.Element = {
    NoPointers.assertContainsNode(n.data)
    NoCalls.assertContainsNode(n.data)
    NoRecords.assertContainsNode(n.data)
    n match {
      case r: CfgStmtNode =>
        r.data match {
          // var declarations
          case varr: AVarStmt => ??? //<--- Complete here

          // assignments
          case AAssignStmt(id: AIdentifier, right, _) => ??? //<--- Complete here

          // all others: like no-ops
          case _ => s
        }
      case _ => s
    }
  }
}

/**
  * Common functionality for interprocedural analysis.
  */
trait InterprocValueAnalysisMisc[N] extends ValueAnalysisMisc {

  implicit val declData: DeclarationData

  val cfg: InterproceduralProgramCfg

  /**
    * Lifted state lattice, with new bottom element representing "unreachable".
    */
  val liftedstatelattice: LiftLattice[statelattice.type]

  /**
    * The analysis lattice.
    */
  val lattice: MapLattice[N, liftedstatelattice.type]

  /**
    * Abstract evaluation of function arguments.
    */
  def evalArgs(formalParams: Seq[ADeclaration], actualParams: Seq[AExpr], state: statelattice.Element): statelattice.Element =
    formalParams.zip(actualParams).foldLeft(statelattice.bottom) {
      case (acc, (id, exp)) =>
        acc + (id -> eval(exp, state))
    }
}

/**
  * Constraint functions for value analysis (including interprocedural).
  * This version is for the basic worklist algorithm.
  */
trait InterprocValueAnalysisFunctions extends MapLiftLatticeSolver[CfgNode] with InterprocValueAnalysisMisc[CfgNode] {

  /**
    * Overrides `funsub` from [[tip.solvers.MapLatticeSolver]] adding support for function calls and returns.
    */
  override def funsub(n: CfgNode, x: lattice.Element): liftedstatelattice.Element = {
    //import cfg._ // gives easy access to the functionality in InterproceduralProgramCfg
    import liftedstatelattice._

    new NormalizedCalls().assertContainsNode(n.data)

    n match {
      // function entry nodes
      case funentry: CfgFunEntryNode => ??? //<--- Complete here

      // after-call nodes
      case aftercall: CfgAfterCallNode => ??? //<--- Complete here

      // return node
      case CfgStmtNode(_, _, _, ret: AReturnStmt) =>
        val j = join(n, x)
        j + (AstOps.returnId -> eval(ret.exp, j))

      // call nodes (like no-ops here)
      case _: CfgCallNode => join(n, x)

      // function exit nodes (like no-ops here)
      case _: CfgFunExitNode => join(n, x)

      // all other nodes
      case _ => super.funsub(n, x)
    }
  }
}

/**
  * Constraint functions for value analysis (including interprocedural), propagation style.
  * This is a variant of [[InterprocValueAnalysisFunctions]] for use with [[tip.solvers.WorklistFixpointPropagationSolver]].
  */
trait InterprocValueAnalysisFunctionsWithPropagation
    extends MapLiftLatticeSolver[CfgNode]
    with WorklistFixpointPropagationFunctions[CfgNode]
    with InterprocValueAnalysisMisc[CfgNode] {

  /**
    * Transfer function for state lattice elements.
    */
  def transferUnlifted(n: CfgNode, s: statelattice.Element): statelattice.Element = {
    import cfg._
    import liftedstatelattice._

    // helper function that propagates dataflow from a function exit node to an after-call node
    def returnflow(funexit: CfgFunExitNode, aftercall: CfgAfterCallNode) =
      x(funexit) match {
        case Lift(exitState) =>
          val newState = x(aftercall.pred.head) + (aftercall.targetIdentifier.declaration -> exitState(AstOps.returnId))
          propagate(newState, aftercall)
        case Bottom => // not (yet) any dataflow at funexit
      }

    n match {
      // call nodes
      case call: CfgCallNode =>
        call.callees.foreach { entry =>
          // build entry state and new call context, then propagate to function entry
          val newState = evalArgs(entry.data.params, call.invocation.args, s)
          propagate(newState, entry)
          // make sure existing return flow gets propagated
          returnflow(entry.exit, call.afterCallNode)
        }
        statelattice.bottom // no flow directly to the after-call node

      // function exit nodes
      case funexit: CfgFunExitNode =>
        for (aftercall <- funexit.callersAfterCall) returnflow(funexit, aftercall)
        statelattice.bottom // no successors for this kind of node, but we have to return something

      // return statement
      case CfgStmtNode(_, _, _, ret: AReturnStmt) =>
        s + (AstOps.returnId -> eval(ret.exp, s))

      // function entry nodes (like no-op here)
      case _: CfgFunEntryNode => s

      // after-call nodes (like no-op here)
      case _: CfgAfterCallNode => s

      // all other nodes
      case _ => localTransfer(n, s)
    }
  }
}

/**
  * Base class for value analysis with simple (non-lifted) lattice.
  */
abstract class SimpleValueAnalysis(val cfg: ProgramCfg)(implicit val decl: DeclarationData) extends FlowSensitiveAnalysis(true) with ValueAnalysisMisc {

  /**
    * The analysis lattice.
    */
  val lattice: MapLattice[CfgNode, statelattice.type] = new MapLattice(statelattice)

  val domain: Set[CfgNode] = cfg.nodes

  /**
    * Transfer function for state lattice elements.
    * (Same as `localTransfer` for simple value analysis.)
    */
  def transfer(n: CfgNode, s: statelattice.Element): statelattice.Element = localTransfer(n, s)
}

/**
  * Base class for value analysis with lifted lattice, where the extra bottom element represents "unreachable".
  */
abstract class LiftedValueAnalysis[P <: ProgramCfg](val cfg: P, stateAfterNode: Boolean)(implicit val declData: DeclarationData)
    extends FlowSensitiveAnalysis(stateAfterNode)
    with MapLatticeSolver[CfgNode]
    with ValueAnalysisMisc {

  /**
    * Lifted state lattice, with new bottom element representing "unreachable".
    */
  val liftedstatelattice: LiftLattice[statelattice.type] = new LiftLattice(statelattice)

  /**
    * The analysis lattice.
    */
  val lattice: MapLattice[CfgNode, liftedstatelattice.type] = new MapLattice(liftedstatelattice)

  val domain: Set[CfgNode] = cfg.nodes

  /**
    * The worklist is initialized with all function entry nodes.
    */
  val first: Set[CfgNode] = cfg.funEntries.values.toSet[CfgNode]

  /**
    * Overrides `funsub` from [[tip.solvers.MapLatticeSolver]], treating function entry nodes as reachable.
    */
  override def funsub(n: CfgNode, x: lattice.Element): liftedstatelattice.Element = {
    import liftedstatelattice._
    n match {
      // function entry nodes are always reachable (if intra-procedural analysis)
      case _: CfgFunEntryNode => lift(statelattice.bottom)
      // all other nodes are processed with join+transfer
      case _ => super.funsub(n, x)
    }
  }
}

/**
  * Functionality for basic analyses with lifted state lattice.
  */
trait LiftedValueAnalysisMisc extends ValueAnalysisMisc {

  /**
    * Transfer function for state lattice elements.
    * (Same as `localTransfer` for basic analyses with lifted state lattice.)
    */
  def transferUnlifted(n: CfgNode, s: statelattice.Element): statelattice.Element = localTransfer(n, s)
}

/**
  * Base class for value analysis with context sensitivity and lifted lattice.
  */
abstract class ContextSensitiveValueAnalysis[C <: CallContext](val cfg: InterproceduralProgramCfg)(implicit val declData: DeclarationData)
    extends FlowSensitiveAnalysis(false)
    with ContextSensitiveForwardDependencies[C]
    with MapLiftLatticeSolver[(C, CfgNode)]
    with WorklistFixpointPropagationSolver[(C, CfgNode)]
    with InterprocValueAnalysisMisc[(C, CfgNode)]
    with CallContextFunctions[C] {

  /**
    * Worklist start locations.
    */
  val first = Set((initialContext, cfg.funEntries(cfg.program.mainFunction)))

  /**
    * The initial lattice element at the start locations is "reachable".
    */
  override def init = liftedstatelattice.Lift(statelattice.bottom)

  /**
    * Lifted state lattice, with new bottom element representing "unreachable".
    */
  val liftedstatelattice = new LiftLattice(statelattice)

  /**
    * The analysis lattice.
    */
  val lattice: MapLattice[(C, CfgNode), liftedstatelattice.type] = new MapLattice(liftedstatelattice)

  /**
    * Collect (reverse) call edges, such that we don't have to search through the global lattice element to find the relevant call contexts.
    */
  val returnEdges = new collection.mutable.HashMap[(C, CfgFunExitNode), collection.mutable.Set[(C, CfgAfterCallNode)]]
  with collection.mutable.MultiMap[(C, CfgFunExitNode), (C, CfgAfterCallNode)]

  /**
    * Transfer function for state lattice elements.
    */
  def transferUnlifted(n: (C, CfgNode), s: statelattice.Element): statelattice.Element = {
    import cfg._
    import liftedstatelattice._

    // helper function that propagates dataflow from a function exit node to an after-call node
    def returnflow(exitContext: C, funexit: CfgFunExitNode, callerContext: C, aftercall: CfgAfterCallNode) =
      x(exitContext, funexit) match {
        case Lift(exitState) =>
          val newState = x(callerContext, aftercall.pred.head) + (aftercall.targetIdentifier.declaration -> exitState(AstOps.returnId))
          propagate(newState, (callerContext, aftercall))
        case Bottom => // not (yet) any dataflow at funexit
      }

    val currentContext = n._1
    n._2 match {
      // call nodes
      case call: CfgCallNode =>
        call.callees.foreach { entry =>
          // build entry state and new call context, then propagate to function entry
          val newState = evalArgs(entry.data.params, call.invocation.args, s)
          val newContext = makeCallContext(currentContext, call, newState, entry)
          propagate(newState, (newContext, entry))
          // record the (reverse) call edge, and make sure existing return flow gets propagated
          returnEdges.addBinding((newContext, entry.exit), (currentContext, call.afterCallNode))
          returnflow(newContext, entry.exit, currentContext, call.afterCallNode)
        }
        statelattice.bottom // no successors for this kind of node, but we have to return something

      // function exit nodes
      case funexit: CfgFunExitNode =>
        returnEdges.get((currentContext, funexit)).foreach {
          _.foreach {
            case (callerContext, aftercall) =>
              returnflow(currentContext, funexit, callerContext, aftercall)
          }
        }
        statelattice.bottom // no successors for this kind of node, but we have to return something

      // return statement
      case CfgStmtNode(_, _, _, ret: AReturnStmt) =>
        s + (AstOps.returnId -> eval(ret.exp, s))

      // function entry nodes (like no-op here)
      case _: CfgFunEntryNode => s

      // after-call nodes (like no-op here)
      case _: CfgAfterCallNode => s

      // all other nodes
      case m => localTransfer(m, s)
    }
  }
}

/**
  * Intraprocedural value analysis that uses [[tip.solvers.SimpleFixpointSolver]].
  */
abstract class IntraprocValueAnalysisSimpleSolver[L <: LatticeWithOps](cfg: IntraproceduralProgramCfg, val valuelattice: L)(
  implicit override val declData: DeclarationData
) extends SimpleValueAnalysis(cfg)
    with SimpleMapLatticeFixpointSolver[CfgNode]
    with ForwardDependencies

/**
  * Intraprocedural value analysis that uses [[tip.solvers.SimpleWorklistFixpointSolver]].
  */
abstract class IntraprocValueAnalysisWorklistSolver[L <: LatticeWithOps](cfg: IntraproceduralProgramCfg, val valuelattice: L)(
  implicit override val declData: DeclarationData
) extends SimpleValueAnalysis(cfg)
    with SimpleWorklistFixpointSolver[CfgNode]
    with ForwardDependencies

/**
  * Intraprocedural value analysis that uses [[tip.solvers.WorklistFixpointSolverWithReachability]],
  * with all function entries as start nodes.
  */
abstract class IntraprocValueAnalysisWorklistSolverWithReachability[L <: LatticeWithOps](cfg: IntraproceduralProgramCfg, val valuelattice: L)(
  implicit override val declData: DeclarationData
) extends LiftedValueAnalysis(cfg, true)
    with LiftedValueAnalysisMisc
    with WorklistFixpointSolverWithReachability[CfgNode]
    with ForwardDependencies

/**
  * Intraprocedural value analysis that uses [[tip.solvers.WorklistFixpointPropagationSolver]].
  */
abstract class IntraprocValueAnalysisWorklistSolverWithReachabilityAndPropagation[L <: LatticeWithOps](cfg: IntraproceduralProgramCfg, val valuelattice: L)(
  implicit override val declData: DeclarationData
) extends LiftedValueAnalysis(cfg, false)
    with LiftedValueAnalysisMisc
    with WorklistFixpointPropagationSolver[CfgNode]
    with ForwardDependencies

/**
  * Interprocedural value analysis that uses [[tip.solvers.WorklistFixpointSolverWithReachability]],
  * with the entry of the main function as the only start node.
  */
abstract class InterprocValueAnalysisWorklistSolverWithReachability[L <: LatticeWithOps](cfg: InterproceduralProgramCfg, val valuelattice: L)(
  implicit override val declData: DeclarationData
) extends LiftedValueAnalysis(cfg, true)
    with LiftedValueAnalysisMisc
    with InterprocValueAnalysisFunctions
    with WorklistFixpointSolverWithReachability[CfgNode]
    with InterproceduralForwardDependencies {

  /**
    * Initialize worklist with the program entry point.
    */
  override val first = Set[CfgNode](cfg.funEntries(cfg.program.mainFunction))
}

/**
  * Interprocedural value analysis that uses [[tip.solvers.WorklistFixpointPropagationSolver]].
  * Note that this class uses [[tip.analysis.ForwardDependencies]] which has no interprocedural outdeps,
  * and it does not use indeps.
  */
abstract class InterprocValueAnalysisWorklistSolverWithReachabilityAndPropagation[L <: LatticeWithOps](cfg: InterproceduralProgramCfg, val valuelattice: L)(
  implicit override val declData: DeclarationData
) extends LiftedValueAnalysis(cfg, false)
    with InterprocValueAnalysisFunctionsWithPropagation
    with WorklistFixpointPropagationSolver[CfgNode]
    with ForwardDependencies {

  /**
    * Initialize worklist with the program entry point.
    */
  override val first = Set[CfgNode](cfg.funEntries(cfg.program.mainFunction))
}

/**
  * Context-sensitive value analysis with call-string approach.
  */
abstract class CallStringValueAnalysis[L <: LatticeWithOps](cfg: InterproceduralProgramCfg, val valuelattice: L)(
  implicit override val declData: DeclarationData
) extends ContextSensitiveValueAnalysis[CallStringContext](cfg)
    with CallStringFunctions {

  override val maxCallStringLength = 2; // overriding default from CallStringFunctions
}

/**
  * Context-sensitive value analysis with functional approach.
  */
abstract class FunctionalValueAnalysis[L <: LatticeWithOps](cfg: InterproceduralProgramCfg, val valuelattice: L)(
  implicit override val declData: DeclarationData
) extends ContextSensitiveValueAnalysis[FunctionalContext](cfg)
    with FunctionalFunctions
