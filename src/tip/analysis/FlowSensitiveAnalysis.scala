package tip.analysis

import tip.cfg._
import tip.ast.AstNodeData.DeclarationData

/**
  * A flow-sensitive analysis.
  * @param stateAfterNode true if the abstract state of a CFG node represents the program point <em>after</em> the node,
  *                       false if represents the program point <em>before</em> the node
  *                       (used when outputting analysis results)
  */
abstract class FlowSensitiveAnalysis(val stateAfterNode: Boolean) extends Analysis[Any]

/**
  * A factory to create a specific flow-sensitive analysis that matches the options.
  */
object FlowSensitiveAnalysis {

  def select(kind: Analysis.Value, options: AnalysisOption.Value, cfg: FragmentCfg)(implicit declData: DeclarationData): Option[FlowSensitiveAnalysis] = {

    val typedCfg = options match {
      case AnalysisOption.`iwlr` | AnalysisOption.`iwlrp` | AnalysisOption.`csiwlrp` | AnalysisOption.`cfiwlrp` | AnalysisOption.`ide` |
          AnalysisOption.`summary` =>
        cfg match {
          case w: InterproceduralProgramCfg => Right(w)
          case _ => throw new RuntimeException(s"Whole CFG needed")
        }
      case _ =>
        cfg match {
          case w: IntraproceduralProgramCfg => Left(w)
          case _ => throw new RuntimeException(s"Intraprocedural CFG needed")
        }
    }

    options match {
      case AnalysisOption.Disabled => None
      case AnalysisOption.`simple` =>
        Some(kind match {
          case Analysis.sign => new SimpleSignAnalysis(typedCfg.left.get); //  same functionality as SignAnalysis.Intraprocedural.SimpleSolver(typedCfg.left.get)
          case Analysis.livevars => new LiveVarsAnalysisSimpleSolver(typedCfg.left.get)
          case Analysis.available => new AvailableExpAnalysisSimpleSolver(typedCfg.left.get)
          //case Analysis.vbusy => new VeryBusyExpAnalysisSimpleSolver(typedCfg.left.get) <--- Complete here
          //case Analysis.reaching => new ReachingDefAnalysisSimpleSolver(typedCfg.left.get) <--- Complete here
          case Analysis.constprop => new ConstantPropagationAnalysis.Intraprocedural.SimpleSolver(typedCfg.left.get)
          case _ => throw new RuntimeException(s"Unsupported solver option `$options` for the analysis $kind")
        })
      case AnalysisOption.`wl` =>
        Some(kind match {
          case Analysis.sign => new SignAnalysis.Intraprocedural.WorklistSolver(typedCfg.left.get)
          case Analysis.livevars => new LiveVarsAnalysisWorklistSolver(typedCfg.left.get)
          case Analysis.available => new AvailableExpAnalysisWorklistSolver(typedCfg.left.get)
          //case Analysis.vbusy => new VeryBusyExpAnalysisWorklistSolver(typedCfg.left.get) <--- Complete here
          //case Analysis.reaching => new ReachingDefAnalysisWorklistSolver(typedCfg.left.get) <--- Complete here
          case Analysis.constprop => new ConstantPropagationAnalysis.Intraprocedural.WorklistSolver(typedCfg.left.get)
          case _ => throw new RuntimeException(s"Unsupported solver option `$options` for the analysis $kind")
        })
      case AnalysisOption.`wlr` =>
        Some(kind match {
          case Analysis.sign => new SignAnalysis.Intraprocedural.WorklistSolverWithReachability(typedCfg.left.get)
          case Analysis.constprop => new ConstantPropagationAnalysis.Intraprocedural.WorklistSolverWithReachability(typedCfg.left.get)
          case _ => throw new RuntimeException(s"Unsupported solver option `$options` for the analysis $kind")
        })
      case AnalysisOption.`wlrw` =>
        Some(kind match {
          case Analysis.interval => new IntervalAnalysis.Intraprocedural.WorklistSolverWithWidening(typedCfg.left.get)
          case _ => throw new RuntimeException(s"Unsupported solver option `$options` for the analysis $kind")
        })
      case AnalysisOption.`wlrwn` =>
        Some(kind match {
          case Analysis.interval => new IntervalAnalysis.Intraprocedural.WorklistSolverWithWideningAndNarrowing(typedCfg.left.get)
          case _ => throw new RuntimeException(s"Unsupported solver option `$options` for the analysis $kind")
        })
      case AnalysisOption.`wlrp` =>
        Some(kind match {
          case Analysis.sign => new SignAnalysis.Intraprocedural.WorklistSolverWithReachabilityAndPropagation(typedCfg.left.get)
          case Analysis.constprop => new ConstantPropagationAnalysis.Intraprocedural.WorklistSolverWithReachabilityAndPropagation(typedCfg.left.get)
          case _ => throw new RuntimeException(s"Unsupported solver option `$options` for the analysis $kind")
        })
      case AnalysisOption.`iwlr` =>
        Some(kind match {
          case Analysis.sign => new SignAnalysis.Interprocedural.WorklistSolverWithReachability(typedCfg.right.get)
          case Analysis.constprop => new ConstantPropagationAnalysis.Interprocedural.WorklistSolverWithReachability(typedCfg.right.get)
          case _ => throw new RuntimeException(s"Unsupported solver option `$options` for the analysis $kind")
        })
      case AnalysisOption.`iwlrp` =>
        Some(kind match {
          case Analysis.sign => new SignAnalysis.Interprocedural.WorklistSolverWithReachabilityAndPropagation(typedCfg.right.get)
          case Analysis.constprop => new ConstantPropagationAnalysis.Interprocedural.WorklistSolverWithReachabilityAndPropagation(typedCfg.right.get)
          case _ => throw new RuntimeException(s"Unsupported solver option `$options` for the analysis $kind")
        })
      case AnalysisOption.`csiwlrp` =>
        Some(kind match {
          case Analysis.sign => new SignAnalysis.Interprocedural.CallString(typedCfg.right.get)
          case Analysis.constprop => new ConstantPropagationAnalysis.Interprocedural.CallString(typedCfg.right.get)
          case _ => throw new RuntimeException(s"Unsupported solver option `$options` for the analysis $kind")
        })
      case AnalysisOption.`cfiwlrp` =>
        Some(kind match {
          case Analysis.sign => new SignAnalysis.Interprocedural.Functional(typedCfg.right.get)
          case Analysis.constprop => new ConstantPropagationAnalysis.Interprocedural.Functional(typedCfg.right.get)
          case _ => throw new RuntimeException(s"Unsupported solver option `$options` for the analysis $kind")
        })
      case AnalysisOption.`ide` =>
        Some(kind match {
          case Analysis.copyconstprop => new CopyConstantPropagationIDEAnalysis(typedCfg.right.get)
          case Analysis.uninitvars => new PossiblyUninitializedVarsIDEAnalysis(typedCfg.right.get)
          case Analysis.taint => new TaintIDEAnalysis(typedCfg.right.get)
          case _ => throw new RuntimeException(s"Unsupported solver option `$options` for the analysis $kind")
        })
      case AnalysisOption.`summary` =>
        Some(kind match {
          case Analysis.copyconstprop => new CopyConstantPropagationSummaryAnalysis(typedCfg.right.get)
          case Analysis.uninitvars => new PossiblyUninitializedVarsSummaryAnalysis(typedCfg.right.get)
          case Analysis.taint => new TaintSummaryAnalysis(typedCfg.right.get)
          case _ => throw new RuntimeException(s"Unsupported solver option `$options` for the analysis $kind")
        })
    }
  }

  /**
    * The options of the analysis:
    *
    * - Enabled: use the simple fixpoint solver
    * - wl: use the worklist solver
    * - wlr: use the worklist solver with reachability
    * - wlrw: use the worklist solver with reachability and widening
    * - wlrwn: use the worklist solver with reachability, widening, and narrowing
    * - wlrp: use the worklist solver with reachability and propagation
    * - iwlr: use the worklist solver with reachability, interprocedural version
    * - iwlrp: use the worklist solver with reachability and propagation, interprocedural version
    * - csiwlrp: use the worklist solver with reachability and propagation, context-sensitive (with call string) interprocedural version
    * - cfiwlrp: use the worklist solver with reachability and propagation, context-sensitive (with functional approach) interprocedural version
    * - ide: use the IDE solver
    * - summary: use the summary solver
    */
  object AnalysisOption extends Enumeration {
    val simple, Disabled, wl, wlr, wlrw, wlrwn, wlrp, iwlr, iwlrp, csiwlrp, cfiwlrp, ide, summary = Value

    def interprocedural(v: Value): Boolean =
      v match {
        case `iwlr` => true
        case `iwlrp` => true
        case `csiwlrp` => true
        case `cfiwlrp` => true
        case `ide` => true
        case `summary` => true
        case _ => false
      }

    def contextsensitive(v: Value): Boolean =
      v match {
        case `csiwlrp` => true
        case `cfiwlrp` => true
        case _ => false
      }

    def withWidening(v: Value): Boolean =
      v match {
        case `wlrw` => true
        case `wlrwn` => true
        case _ => false
      }
  }

  /**
    * A flow sensitive analysis kind
    */
  object Analysis extends Enumeration {
    val sign, livevars, available, vbusy, reaching, constprop, interval, copyconstprop, uninitvars, taint = Value
  }
}
