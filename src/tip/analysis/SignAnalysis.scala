package tip.analysis

import tip.ast._
import tip.cfg.CfgOps._
import tip.cfg._
import tip.lattices.{SignLattice, _}
import tip.solvers._
import tip.ast.AstNodeData.{AstNodeWithDeclaration, DeclarationData}

/**
  * Transfer functions for sign analysis (intraprocedural only).
  */
trait IntraprocSignAnalysisFunctions {

  implicit val declData: DeclarationData

  type Sign = SignElement.Value

  val domain: Set[CfgNode]

  val declaredVars = domain.flatMap(_.declaredVarsAndParams)

  val statelattice = new MapLattice(declaredVars, SignLattice)

  /**
    * The transfer functions.
    */
  def localTransfer(n: CfgNode, s: statelattice.Element): statelattice.Element = {
    NoPointers.assertContainsNode(n.data)
    NoCalls.assertContainsNode(n.data)
    n match {
      case r: CfgStmtNode =>
        r.data match {
          // var declarations
          case varr: AVarStmt => ??? //<--- Complete here

          // assignments
          case AAssignStmt(Left(id), right, _) => ??? //<--- Complete here
          case AAssignStmt(Right(_), _, _) => NoPointers.LanguageRestrictionViolation(s"${r.data} not allowed")

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
trait InterprocSignAnalysisMisc[N] {

  val lattice: MapLattice[N, LiftLattice[MapLattice[ADeclaration, SignLattice.type]]]

  val cfg: InterproceduralProgramCfg

  implicit val declData: DeclarationData

  def evalArgs(formalParams: Seq[ADeclaration],
               actualParams: Seq[AExpr],
               state: lattice.sublattice.sublattice.Element): lattice.sublattice.sublattice.Element = {
    formalParams.zip(actualParams).foldLeft(lattice.sublattice.sublattice.bottom) {
      case (acc, (id, exp)) =>
        acc + (id -> lattice.sublattice.sublattice.sublattice.eval(exp, state))
    }
  }
}

/**
  * Constraint functions for sign analysis (including interprocedural).
  * This version is for the basic worklist algorithm.
  */
trait InterprocSignAnalysisFunctions
    extends MapLiftLatticeSolver[CfgNode]
    with InterprocSignAnalysisMisc[CfgNode]
    with InterproceduralForwardDependencies {

  override def funsub(n: CfgNode, x: lattice.Element): lattice.sublattice.Element = {
    import lattice.sublattice._
    import cfg._

    NormalizedCalls().assertContainsNode(n.data)

    n match {
      // function entry nodes
      case funentry: CfgFunEntryNode => ??? //<--- Complete here

      // after-call nodes
      case aftercall: CfgAfterCallNode => ??? //<--- Complete here

      // return node
      case CfgStmtNode(_, _, _, ret: AReturnStmt) =>
        val j = join(n, x)
        j + (AstOps.returnId -> lattice.sublattice.sublattice.sublattice.eval(ret.value, j))

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
  * Constraint functions for sign analysis (including interprocedural), propagation style.
  * This is a variant of [[InterprocSignAnalysisFunctions]] for use with [[tip.solvers.WorklistFixpointPropagationSolver]].
  */
trait InterprocSignAnalysisFunctionsWithPropagation
    extends MapLiftLatticeSolver[CfgNode]
    with WorklistFixpointPropagationSolver[CfgNode]
    with InterprocSignAnalysisMisc[CfgNode]
    with IntraprocSignAnalysisFunctions {

  override def transferUnlifted(n: CfgNode, s: statelattice.Element): statelattice.Element = {
    import cfg._
    import lattice.sublattice._

    // helper function that propagates dataflow from a function exit node to an after-call node
    def returnflow(funexit: CfgFunExitNode, aftercall: CfgAfterCallNode) {
      x(funexit) match {
        case Lift(exitState) =>
          val newState = x(aftercall.pred.head) + (aftercall.targetIdentifier.declaration -> exitState(AstOps.returnId))
          propagate(newState, aftercall)
        case Bottom => // not (yet) any dataflow at funexit
      }
    }

    n match {
      // call nodes
      case call: CfgCallNode =>
        call.callees.foreach { entry =>
          // build entry state and new call context, then propagate to function entry
          val newState = evalArgs(entry.data.args, call.invocation.args, s)
          propagate(newState, entry)
          // make sure existing return flow gets propagated
          returnflow(entry.exit, call.afterCallNode)
        }
        lattice.sublattice.sublattice.bottom // no flow directly to the after-call node

      // function exit nodes
      case funexit: CfgFunExitNode =>
        for (aftercall <- funexit.callersAfterCall) returnflow(funexit, aftercall)
        lattice.sublattice.sublattice.bottom // no successors for this kind of node, but we have to return something

      // return statement
      case CfgStmtNode(_, _, _, ret: AReturnStmt) =>
        s + (AstOps.returnId -> lattice.sublattice.sublattice.sublattice.eval(ret.value, s))

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
  * Context-sensitive variant of [[InterprocSignAnalysisFunctionsWithPropagation]].
  * @tparam C type of call contexts
  */
trait ContextSensitiveSignAnalysisFunctions[C <: CallContext]
    extends MapLiftLatticeSolver[(C, CfgNode)]
    with WorklistFixpointPropagationSolver[(C, CfgNode)]
    with InterprocSignAnalysisMisc[(C, CfgNode)]
    with IntraprocSignAnalysisFunctions
    with CallContextFunctions[C, MapLattice[ADeclaration, SignLattice.type]] {

  /**
    * Collect (reverse) call edges, such that we don't have to search through the global lattice element to find the relevant call contexts.
    */
  val returnEdges = new collection.mutable.HashMap[(C, CfgFunExitNode), collection.mutable.Set[(C, CfgAfterCallNode)]]
  with collection.mutable.MultiMap[(C, CfgFunExitNode), (C, CfgAfterCallNode)]

  override def transferUnlifted(n: (C, CfgNode), s: statelattice.Element): statelattice.Element = {
    import cfg._
    import lattice.sublattice._

    // helper function that propagates dataflow from a function exit node to an after-call node
    def returnflow(exitContext: C, funexit: CfgFunExitNode, callerContext: C, aftercall: CfgAfterCallNode) {
      x(exitContext, funexit) match {
        case Lift(exitState) =>
          val newState = x(callerContext, aftercall.pred.head) + (aftercall.targetIdentifier.declaration -> exitState(AstOps.returnId))
          propagate(newState, (callerContext, aftercall))
        case Bottom => // not (yet) any dataflow at funexit
      }
    }

    val currentContext = n._1
    n._2 match {
      // call nodes
      case call: CfgCallNode =>
        call.callees.foreach { entry =>
          // build entry state and new call context, then propagate to function entry
          val newState = evalArgs(entry.data.args, call.invocation.args, s)
          val newContext = makeCallContext(currentContext, call, newState, entry)
          propagate(newState, (newContext, entry))
          // record the (reverse) call edge, and make sure existing return flow gets propagated
          returnEdges.addBinding((newContext, entry.exit), (currentContext, call.afterCallNode))
          returnflow(newContext, entry.exit, currentContext, call.afterCallNode)
        }
        lattice.sublattice.sublattice.bottom // no successors for this kind of node, but we have to return something

      // function exit nodes
      case funexit: CfgFunExitNode =>
        returnEdges.get((currentContext, funexit)).foreach {
          _.foreach {
            case (callerContext, aftercall) =>
              returnflow(currentContext, funexit, callerContext, aftercall)
          }
        }
        lattice.sublattice.sublattice.bottom // no successors for this kind of node, but we have to return something

      // return statement
      case CfgStmtNode(_, _, _, ret: AReturnStmt) =>
        s + (AstOps.returnId -> lattice.sublattice.sublattice.sublattice.eval(ret.value, s))

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
  * Base class for sign analysis with simple (non-lifted) lattice.
  */
abstract class SimpleSignAnalysis(cfg: ProgramCfg)(implicit val declData: DeclarationData)
    extends FlowSensitiveAnalysis[CfgNode](cfg)
    with IntraprocSignAnalysisFunctions
    with ForwardDependencies {

  val lattice = new MapLattice(cfg.nodes, statelattice)

  def transfer(n: CfgNode, s: lattice.sublattice.Element): lattice.sublattice.Element = localTransfer(n, s)
}

/**
  * Base class for sign analysis with lifted lattice.
  */
abstract class LiftedSignAnalysis(cfg: ProgramCfg)(implicit val declData: DeclarationData)
    extends FlowSensitiveAnalysis[CfgNode](cfg)
    with MapLatticeSolver[CfgNode]
    with IntraprocSignAnalysisFunctions
    with ForwardDependencies {

  val lattice = new MapLattice(cfg.nodes, new LiftLattice(statelattice))

  val first = cfg.funEntries.values.toSet[CfgNode]

  override def funsub(n: CfgNode, x: lattice.Element): lattice.sublattice.Element = {
    import lattice.sublattice._
    n match {
      // function entry nodes are always reachable (if intra-procedural analysis)
      case funentry: CfgFunEntryNode => lift(lattice.sublattice.sublattice.bottom)
      // all other nodes are processed with join+transfer
      case _ => super.funsub(n, x)
    }
  }
}

/**
  * Base class for sign analysis with context sensitivity and lifted lattice.
  */
abstract class ContextSensitiveSignAnalysis[C <: CallContext](cfg: InterproceduralProgramCfg)(implicit val declData: DeclarationData)
    extends FlowSensitiveAnalysis[(C, CfgNode)](cfg)
    with ContextSensitiveSignAnalysisFunctions[C]
    with ContextSensitiveForwardDependencies[C] {

  val lattice = new MapLattice({ _: (C, CfgNode) =>
    true // in principle, we should check that the node is in the CFG, but this function is not called anyway...
  }, new LiftLattice(statelattice))
}

/**
  * Intraprocedural sign analysis that uses [[tip.solvers.SimpleFixpointSolver]].
  */
class IntraprocSignAnalysisSimpleSolver(cfg: ProgramCfg)(override implicit val declData: DeclarationData)
    extends SimpleSignAnalysis(cfg)
    with SimpleMapLatticeFixpointSolver[CfgNode]

/**
  * Intraprocedural sign analysis that uses [[tip.solvers.SimpleWorklistFixpointSolver]].
  */
class IntraprocSignAnalysisWorklistSolver(cfg: ProgramCfg)(override implicit val declData: DeclarationData)
    extends SimpleSignAnalysis(cfg)
    with SimpleWorklistFixpointSolver[CfgNode]

/**
  * Intraprocedural sign analysis that uses [[tip.solvers.WorklistFixpointSolverWithInit]],
  * with all function entries as start nodes.
  */
class IntraprocSignAnalysisWorklistSolverWithInit(cfg: ProgramCfg)(override implicit val declData: DeclarationData)
    extends LiftedSignAnalysis(cfg)
    with WorklistFixpointSolverWithInit[CfgNode] {

  def transferUnlifted(n: CfgNode, s: lattice.sublattice.sublattice.Element): lattice.sublattice.sublattice.Element = localTransfer(n, s)
}

/**
  * Intraprocedural sign analysis that uses [[tip.solvers.WorklistFixpointPropagationSolver]].
  */
class IntraprocSignAnalysisWorklistSolverWithInitAndPropagation(cfg: ProgramCfg)(override implicit val declData: DeclarationData)
    extends IntraprocSignAnalysisWorklistSolverWithInit(cfg)
    with WorklistFixpointPropagationSolver[CfgNode]

/**
  * Interprocedural sign analysis that uses [[tip.solvers.WorklistFixpointSolverWithInit]].
  */
class InterprocSignAnalysisWorklistSolverWithInit(val cfg: InterproceduralProgramCfg)(override implicit val declData: DeclarationData)
    extends IntraprocSignAnalysisWorklistSolverWithInit(cfg)
    with InterprocSignAnalysisFunctions
    with InterproceduralForwardDependencies {

  override val first = Set[CfgNode](cfg.funEntries(cfg.program.mainFunction))
}

/**
  * Interprocedural sign analysis that uses [[tip.solvers.WorklistFixpointPropagationSolver]].
  * Note that this class uses [[tip.analysis.ForwardDependencies]] which has no interprocedural outdeps,
  * and it does not use indeps.
  */
class InterprocSignAnalysisWorklistSolverWithInitAndPropagation(val cfg: InterproceduralProgramCfg)(override implicit val declData: DeclarationData)
    extends IntraprocSignAnalysisWorklistSolverWithInitAndPropagation(cfg)
    with InterprocSignAnalysisFunctionsWithPropagation
    with ForwardDependencies {

  override val first = Set[CfgNode](cfg.funEntries(cfg.program.mainFunction))
}

/**
  * Context-sensitive sign analysis with call-string approach.
  */
class CallStringSignAnalysis(val cfg: InterproceduralProgramCfg)(override implicit val declData: DeclarationData)
    extends ContextSensitiveSignAnalysis[CallStringContext](cfg)
    with CallStringFunctions[MapLattice[ADeclaration, SignLattice.type]] {

  override def init = lattice.sublattice.Lift(lattice.sublattice.sublattice.bottom)

  override val first = Set[(CallStringContext, CfgNode)]((initialContext, cfg.funEntries(cfg.program.mainFunction)))

  override val maxCallStringLength = 2; // overriding default from CallStringFunctions
}

/**
  * Context-sensitive sign analysis with functional approach.
  */
class FunctionalSignAnalysis(val cfg: InterproceduralProgramCfg)(override implicit val declData: DeclarationData)
    extends ContextSensitiveSignAnalysis[FunctionalContext](cfg)
    with FunctionalFunctions[MapLattice[ADeclaration, SignLattice.type]] {

  override def init = lattice.sublattice.Lift(lattice.sublattice.sublattice.bottom)

  override val first = Set[(FunctionalContext, CfgNode)]((initialContext, cfg.funEntries(cfg.program.mainFunction)))
}
