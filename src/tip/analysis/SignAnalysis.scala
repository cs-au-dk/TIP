package tip.analysis

import tip.ast._
import tip.cfg.CfgOps._
import tip.cfg._
import tip.lattices.{SignLattice, _}
import tip.solvers._
import tip.ast.AstNodeData.{AstNodeWithDeclaration, DeclarationData}

import tip.util.Log

object SignAnalysisCommons {
  val returnId = AIdentifierDeclaration(s"#result", Loc(0, 0))
}

/**
  * Transfer functions for sign analysis (intraprocedural only).
  */
trait IntraprocSignAnalysisTransferFunctions {

  implicit val declData: DeclarationData

  val log = Log.logger[this.type](Log.Level.None)

  type Sign = SignElement.Value

  val domain: Set[CfgNode]

  val declaredVars = domain.flatMap(_.declaredVarsAndParams)

  val statelattice = new MapLattice(declaredVars, SignLattice)

  /**
    * The transfer functions.
    */
  def transfer(n: CfgNode, s: statelattice.Element): statelattice.Element = {
    NoPointers.assertContainsNode(n.data)
    NoCalls.assertContainsNode(n.data)
    n match {
      case r: CfgStmtNode =>
        r.data match {

          // var declarations
          case varr: AVarStmt => ??? //<--- Complete here

          // assignments
          case AAssignStmt(Left(id), right, _) => ??? //<--- Complete here

          case AAssignStmt(Right(_), _, _) => NoPointers.LanguageRestrictionViolation(s"De-reference on the right-hand of ${r.data}")

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
trait InterprocSignAnalysisMisc {

  val lattice: MapLattice[CfgNode, LiftLattice[MapLattice[ADeclaration, SignLattice.type]]]

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
  * Update function for sign analysis (including interprocedural).
  * This version is for the basic worklist algorithm.
  */
trait InterprocSignAnalysisFunctions
    extends MapLiftLatticeUpdateFunction[CfgNode]
    with InterprocSignAnalysisMisc
    with InterproceduralForwardDependencies {

  private val log = Log.logger[this.type](Log.Level.None)

  implicit val declData: DeclarationData

  override def funsub(n: CfgNode, s: lattice.sublattice.Element, o: lattice.Element): lattice.sublattice.Element = {
    import lattice.sublattice._
    import cfg._

    log.debug(s"Processing $n ${n.getClass}")

    NormalizedCalls().assertContainsNode(n.data)

    n match {

      // call nodes (like no-ops here)
      case _: CfgCallNode => join(n, s, o)

      // function entry nodes
      case funentry: CfgFunEntryNode => ??? //<--- Complete here

      // function exit nodes (like no-ops here)
      case _: CfgFunExitNode => join(n, s, o)

      // after-call nodes
      case aftercall: CfgAfterCallNode => ??? //<--- Complete here

      case CfgStmtNode(_, _, _, ret: AReturnStmt) =>
        val j = join(n, s, o)
        j + (SignAnalysisCommons.returnId -> lattice.sublattice.sublattice.sublattice.eval(ret.value, j))

      case _ => super.funsub(n, s, o)
    }
  }
}

/**
  * Update functions for sign analysis (including interprocedural), propagation style.
  * This is a variant of [[InterprocSignAnalysisFunctions]] for use with [[tip.solvers.WorklistFixpointPropagationSolver]].
  */
trait InterprocSignAnalysisFunctionsWithPropagation
    extends MapLiftLatticeUpdateFunction[CfgNode]
    with WorklistFixpointPropagationSolver[CfgNode]
    with InterprocSignAnalysisMisc {

  implicit val declData: DeclarationData

  override def funsub(n: CfgNode, s: lattice.sublattice.Element, o: lattice.Element): lattice.sublattice.Element = {
    import cfg._
    import lattice.sublattice._

    // helper function that propagates dataflow from a function exit node to an after-call node
    def returnflow(funexit: CfgFunExitNode, aftercall: CfgAfterCallNode) {
      val id = aftercall.targetIdentifier
      propagate(o(aftercall.pred.head) + (id.declaration -> s(SignAnalysisCommons.returnId)), aftercall)
    }

    n match {

      // call nodes
      case call: CfgCallNode =>
        val calledEntries = call.callees
        calledEntries.foreach { entry =>
          propagate(evalArgs(entry.data.args, call.invocation.args, s), entry)
          returnflow(entry.exit, call.afterCallNode) // make sure existing return flow gets propagated
        }
        lattice.sublattice.bottom // no flow directly to the after-call node

      // function entry nodes (like no-op here)
      case _: CfgFunEntryNode => s

      // function exit nodes
      case funexit: CfgFunExitNode =>
        for (aftercall <- funexit.callersAfterCall) returnflow(funexit, aftercall)
        lattice.sublattice.bottom // no successors for this kind of node, but we have to return something

      // after-call nodes (like no-op here)
      case _: CfgAfterCallNode => s

      case CfgStmtNode(_, _, _, ret: AReturnStmt) =>
        s + (SignAnalysisCommons.returnId -> lattice.sublattice.sublattice.sublattice.eval(ret.value, s))

      case _ => super.funsub(n, s, o)
    }
  }
}

/**
  * Base class for sign analysis.
  */
abstract class IntraprocSignAnalysis(cfg: ProgramCfg)
    extends FlowSensitiveAnalysis(cfg)
    with IntraprocSignAnalysisTransferFunctions
    with MapLiftLatticeUpdateFunction[CfgNode]
    with ForwardDependencies {

  val lattice = new MapLattice(cfg.nodes, new LiftLattice(statelattice))
}

/**
  * Intraprocedural sign analysis that uses [[tip.solvers.SimpleFixpointSolver]].
  */
class IntraprocSignAnalysisSimpleSolver(cfg: ProgramCfg)(override implicit val declData: DeclarationData)
    extends IntraprocSignAnalysis(cfg)
    with SimpleFixpointSolver
    with MapLatticeUpdateFunction[CfgNode]

/**
  * Intraprocedural sign analysis that uses [[tip.solvers.WorklistFixpointSolver]].
  */
class IntraprocSignAnalysisWorklistSolver(cfg: ProgramCfg)(implicit val declData: DeclarationData)
    extends IntraprocSignAnalysis(cfg)
    with WorklistFixpointSolver[CfgNode]

/**
  * Intraprocedural sign analysis that uses [[tip.solvers.WorklistFixpointSolverWithInit]],
  * with all function entries as start nodes.
  */
class IntraprocSignAnalysisWorklistSolverWithInit(cfg: ProgramCfg)(override implicit val declData: DeclarationData)
    extends IntraprocSignAnalysisWorklistSolver(cfg)
    with WorklistFixpointSolverWithInit[CfgNode] {

  val first = cfg.funEntries.values.toSet[CfgNode]
}

/**
  * Intraprocedural sign analysis that uses [[tip.solvers.WorklistFixpointSolverWithInit]]
  * together with [[tip.solvers.WorklistFixpointPropagationSolver]].
  */
class IntraprocSignAnalysisWorklistSolverWithInitAndPropagation(cfg: ProgramCfg)(override implicit val declData: DeclarationData)
    extends IntraprocSignAnalysisWorklistSolverWithInit(cfg)
    with WorklistFixpointPropagationSolver[CfgNode] {

  override val init = lattice.sublattice.Lift(lattice.sublattice.sublattice.bottom)
}

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
  * Interprocedural sign analysis that uses [[tip.solvers.WorklistFixpointSolverWithInit]]
  * together with [[tip.solvers.WorklistFixpointPropagationSolver]].
  * Note that this class uses [[tip.analysis.ForwardDependencies]] which has no interprocedural outdeps,
  * and it does not use indeps.
  */
class InterprocSignAnalysisWorklistSolverWithInitAndPropagation(val cfg: InterproceduralProgramCfg)(override implicit val declData: DeclarationData)
    extends IntraprocSignAnalysisWorklistSolverWithInitAndPropagation(cfg)
    with InterprocSignAnalysisFunctionsWithPropagation
    with ForwardDependencies {

  override val first = Set[CfgNode](cfg.funEntries(cfg.program.mainFunction))
}
