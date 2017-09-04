package tip.analysis

import tip.cfg._
import tip.lattices.{Lattice, MapLattice}
import tip.ast.AstNodeData.DeclarationData

/**
  * A flow-sensitive analysis.
  */
abstract class FlowSensitiveAnalysis[N](cfg: FragmentCfg) extends Analysis[Any] {

  /**
    * The lattice used by the analysis.
    */
  val lattice: MapLattice[N, Lattice]

  /**
    * The domain of the map lattice.
    */
  val domain = cfg.nodes

  /**
    * @inheritdoc
    */
  def analyze(): lattice.Element
}

/**
  * A factory to create a specific flow-sensitive analysis that matches the options.
  */
object FlowSensitiveAnalysis {

  def select(kind: Analysis.Value, options: AnalysisOption.Value, cfg: FragmentCfg)(implicit declData: DeclarationData): Option[FlowSensitiveAnalysis[_]] = {

    val typedCfg = options match {
      case AnalysisOption.iwli | AnalysisOption.iwlip | AnalysisOption.`csiwlip` | AnalysisOption.`cfiwlip` | AnalysisOption.`ide` =>
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
          case Analysis.sign => new IntraprocSignAnalysisSimpleSolver(typedCfg.left.get)
          case Analysis.livevars => new LiveVarsAnalysisSimpleSolver(typedCfg.left.get)
          case Analysis.available => new AvailableExpAnalysisSimpleSolver(typedCfg.left.get)
          //case Analysis.vbusy => new VeryBusyExpAnalysisSimpleSolver(typedCfg.left.get) <--- Complete here
          //case Analysis.reaching => new ReachingDefAnalysisSimpleSolver(typedCfg.left.get) <--- Complete here
          case Analysis.constprop => new ConstantPropagationAnalysisSimpleSolver(typedCfg.left.get)
          case _ => throw new RuntimeException(s"Unsupported solver option `$options` for the analysis $kind")
        })
      case AnalysisOption.`wl` =>
        Some(kind match {
          case Analysis.sign => new IntraprocSignAnalysisWorklistSolver(typedCfg.left.get)
          case Analysis.livevars => new LiveVarsAnalysisWorklistSolver(typedCfg.left.get)
          case Analysis.available => new AvailableExpAnalysisWorklistSolver(typedCfg.left.get)
          //case Analysis.vbusy => new VeryBusyExpAnalysisWorklistSolver(typedCfg.left.get) <--- Complete here
          //case Analysis.reaching => new ReachingDefAnalysisWorklistSolver(typedCfg.left.get) <--- Complete here
          case Analysis.constprop => new ConstantPropagationAnalysisWorklistSolver(typedCfg.left.get)
          case _ => throw new RuntimeException(s"Unsupported solver option `$options` for the analysis $kind")
        })
      case AnalysisOption.`wli` =>
        Some(kind match {
          case Analysis.sign => new IntraprocSignAnalysisWorklistSolverWithInit(typedCfg.left.get)
          case Analysis.interval => new IntervalAnalysisWorklistSolverWithInit(typedCfg.left.get)
          case _ => throw new RuntimeException(s"Unsupported solver option `$options` for the analysis $kind")
        })
      case AnalysisOption.`wliw` =>
        Some(kind match {
          case Analysis.interval => new IntervalAnalysisWorklistSolverWithWidening(typedCfg.left.get)
          case _ => throw new RuntimeException(s"Unsupported solver option `$options` for the analysis $kind")
        })
      case AnalysisOption.`wliwn` =>
        Some(kind match {
          case Analysis.interval => new IntervalAnalysisWorklistSolverWithWideningAndNarrowing(typedCfg.left.get)
          case _ => throw new RuntimeException(s"Unsupported solver option `$options` for the analysis $kind")
        })
      case AnalysisOption.`wlip` =>
        Some(kind match {
          case Analysis.sign => new IntraprocSignAnalysisWorklistSolverWithInitAndPropagation(typedCfg.left.get)
          case _ => throw new RuntimeException(s"Unsupported solver option `$options` for the analysis $kind")
        })
      case AnalysisOption.`iwli` =>
        Some(kind match {
          case Analysis.sign => new InterprocSignAnalysisWorklistSolverWithInit(typedCfg.right.get)
          case _ => throw new RuntimeException(s"Unsupported solver option `$options` for the analysis $kind")
        })
      case AnalysisOption.`iwlip` =>
        Some(kind match {
          case Analysis.sign => new InterprocSignAnalysisWorklistSolverWithInitAndPropagation(typedCfg.right.get)
          case _ => throw new RuntimeException(s"Unsupported solver option `$options` for the analysis $kind")
        })
      case AnalysisOption.`csiwlip` =>
        Some(kind match {
          case Analysis.sign => new CallStringSignAnalysis(typedCfg.right.get)
          case _ => throw new RuntimeException(s"Unsupported solver option `$options` for the analysis $kind")
        })
      case AnalysisOption.`cfiwlip` =>
        Some(kind match {
          case Analysis.sign => new FunctionalSignAnalysis(typedCfg.right.get)
          case _ => throw new RuntimeException(s"Unsupported solver option `$options` for the analysis $kind")
        })
      case AnalysisOption.`ide` =>
        Some(kind match {
          case Analysis.copyconstprop => new CopyConstantPropagationIDEAnalysis(typedCfg.right.get)
          case _ => throw new RuntimeException(s"Unsupported solver option `$options` for the analysis $kind")
        })
    }
  }

  /**
    * The options of the analysis:
    *
    * - Enabled: use the simple fixpoint solver
    * - wl: use the worklist solver
    * - wli: use the worklist solver with init
    * - wliw: use the worklist solver with init and widening
    * - wliwn: use the worklist solver with init, widening, and narrowing
    * - wlip: use the worklist solver with init and propagation
    * - iwli: use the worklist solver with init, interprocedural version
    * - iwlip: use the worklist solver with init and propagation, interprocedural version
    * - csiwlip: use the worklist solver with init and propagation, context-sensitive (with call string) interprocedural version
    * - cfiwlip: use the worklist solver with init and propagation, context-sensitive (with functional approach) interprocedural version
    * - ide: use the IDE solver
    */
  object AnalysisOption extends Enumeration {
    val simple, Disabled, wl, wli, wliw, wliwn, wlip, iwli, iwlip, csiwlip, cfiwlip, ide = Value

    def interprocedural(v: Value): Boolean = {
      v match {
        case `iwli` => true
        case `iwlip` => true
        case `csiwlip` => true
        case `cfiwlip` => true
        case `ide` => true
        case _ => false
      }
    }

    def contextsensitive(v: Value): Boolean = {
      v match {
        case `csiwlip` => true
        case `cfiwlip` => true
        case _ => false
      }
    }

    def withWidening(v: Value): Boolean = {
      v match {
        case `wliw` => true
        case `wliwn` => true
        case _ => false
      }
    }
  }

  /**
    * A flow sensitive analysis kind
    */
  object Analysis extends Enumeration {
    val sign, livevars, available, vbusy, reaching, constprop, interval, copyconstprop = Value
  }
}
