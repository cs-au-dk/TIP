package tip.analysis

import tip.cfg._
import tip.lattices.{Lattice, MapLattice}
import tip.ast.AstNodeData.DeclarationData

/**
  * A flow-sensitive analysis.
  */
abstract class FlowSensitiveAnalysis(cfg: FragmentCfg) extends Analysis[Any] {

  /**
    * The lattice used by the analysis.
    */
  val lattice: MapLattice[CfgNode, Lattice]

  /**
    * The domain of the map lattice.
    */
  val domain = cfg.nodes

  /**
    * The local update function.
    */
  def funsub(n: CfgNode, s: lattice.sublattice.Element, o: lattice.Element): lattice.sublattice.Element

  /**
    * @inheritdoc
    */
  def analyze(): lattice.Element
}

/**
  * A factory to create a specific flow-sensitive analysis that matches the options.
  */
object FlowSensitiveAnalysis {

  def select(kind: Analysis.Value, options: AnalysisOption.Value, cfg: FragmentCfg)(
    implicit declData: DeclarationData
  ): Option[FlowSensitiveAnalysis] = {

    val typedCfg = options match { // FIXME
      case AnalysisOption.iwli | AnalysisOption.iwlip =>
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
          case _ => throw new RuntimeException(s"Unsupported option for the analysis $kind")
        })
      case AnalysisOption.`wl` =>
        Some(kind match {
          case Analysis.sign => new IntraprocSignAnalysisWorklistSolver(typedCfg.left.get)
          case Analysis.livevars => new LiveVarsAnalysisWorklistSolver(typedCfg.left.get)
          case Analysis.available => new AvailableExpAnalysisWorklistSolver(typedCfg.left.get)
          //case Analysis.vbusy => new VeryBusyExpAnalysisWorklistSolver(typedCfg.left.get) <--- Complete here
          //case Analysis.reaching => new ReachingDefAnalysisWorklistSolver(typedCfg.left.get) <--- Complete here
          case Analysis.constprop => new ConstantPropagationAnalysisWorklistSolver(typedCfg.left.get)
          case _ => throw new RuntimeException(s"Unsupported option for the analysis $kind")
        })
      case AnalysisOption.`wli` =>
        Some(kind match {
          case Analysis.sign => new IntraprocSignAnalysisWorklistSolverWithInit(typedCfg.left.get)
          case _ => throw new RuntimeException(s"Unsupported option for the analysis $kind")
        })
      case AnalysisOption.`wliw` =>
        Some(kind match {
          case Analysis.interval => new IntervalAnalysisWorklistSolverWithWidening(typedCfg.left.get)
          case _ => throw new RuntimeException(s"Unsupported option for the analysis $kind")
        })
      case AnalysisOption.`wliwn` =>
        Some(kind match {
          case Analysis.interval => new IntervalAnalysisWorklistSolverWithWideningAndNarrowing(typedCfg.left.get)
          case _ => throw new RuntimeException(s"Unsupported option for the analysis $kind")
        })
      case AnalysisOption.`wlip` =>
        Some(kind match {
          case Analysis.sign => new IntraprocSignAnalysisWorklistSolverWithInitAndPropagation(typedCfg.left.get)
          case _ => throw new RuntimeException(s"Unsupported option for the analysis $kind")
        })
      case AnalysisOption.`iwli` =>
        Some(kind match {
          case Analysis.sign => new InterprocSignAnalysisWorklistSolverWithInit(typedCfg.right.get)
          case _ => throw new RuntimeException(s"Unsupported option for the analysis $kind")
        })
      case AnalysisOption.`iwlip` =>
        Some(kind match {
          case Analysis.sign => new InterprocSignAnalysisWorklistSolverWithInitAndPropagation(typedCfg.right.get)
          case _ => throw new RuntimeException(s"Unsupported option for the analysis $kind")
        })
    }
  }

  /**
    * The options of the analysis:
    *
    * - Enabled: tries to create an analysis which uses the simple fixpoint solver
    * - wl: tries to create an analysis which uses the worklist solver
    * - wli: tries to create an analysis which uses the worklist solver with init
    * - wliw: tries to create an analysis which uses the worklist solver with init and widening
    * - wliwn: tries to create an analysis which uses the worklist solver with init, widening, and narrowing
    * - wlip: tries to create an analysis which uses the worklist solver with init and propagation
    * - iwli: tries to create an analysis which uses the worklist solver with init, interprocedural version
    * - iwlip: tries to create an analysis which uses the worklist solver with init and propagation, interprocedural version
    */
  object AnalysisOption extends Enumeration {
    val simple, Disabled, wl, wli, wliw, wliwn, wlip, iwli, iwlip = Value

    def interprocedural(v: Value): Boolean = {
      v match {
        case `iwli` => true
        case `iwlip` => true
        case _ => false
      }
    }
  }

  /**
    * A flow sensitive analysis kind
    */
  object Analysis extends Enumeration {
    val sign, livevars, available, vbusy, reaching, constprop, interval = Value
  }
}
