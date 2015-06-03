package tip.analysis

import tip.ast.AstNode
import tip.graph.{ProgramCFG, GNode, ControlFlowGraph}
import tip.lattices.{Lattice, MapLattice}
import tip.solvers.Dependencies

/**
 * A flow-sensitive analysis.
 */
abstract class FlowSensitiveAnalysis(cfg: ControlFlowGraph[AstNode]) {

  /**
   * The lattice used by the analysis.
   */
  val lattice: MapLattice[GNode[AstNode], Lattice]

  /**
   * The domain of the map lattice.
   */
  val domain = cfg.nodes

  /**
   * The local update function.
   */
  def funsub(n: GNode[AstNode], s: lattice.sublattice.Element, o: lattice.Element): lattice.sublattice.Element

  /**
   * Performs the analysis.
   */
  def analyze(): lattice.Element
}

/**
 * Dependency methods for forward analyses.
 */
trait ForwardDependencies extends Dependencies[GNode[AstNode]] {
  
  def outdep(n: GNode[AstNode]) = n.succ.toSet

  def indep(n: GNode[AstNode]) = n.pred.toSet
}

/**
 * Dependency methods for backward analyses.
 */
trait BackwardDependencies extends Dependencies[GNode[AstNode]] {
  
  def outdep(n: GNode[AstNode]) = n.pred.toSet

  def indep(n: GNode[AstNode]) = n.succ.toSet
}

/**
 * A factory to create a specific flow-sensitive analysis that matches the options.
 */
object FlowSensitiveAnalysis {

  def select(kind: Analysis.Value,
             options: AnalysisOption.Value,
             cfg: ControlFlowGraph[AstNode]): Option[FlowSensitiveAnalysis] = {

    options match {
      case AnalysisOption.Disabled => None
      case AnalysisOption.`simple` =>
        Some(kind match {
          case Analysis.sign => new IntraprocSignAnalysisSimpleSolver(cfg)
          case Analysis.livevars => new LiveVarsAnalysisSimpleSolver(cfg)
          case Analysis.available => new AvailableExpAnalysisSimpleSolver(cfg)
          //case Analysis.vbusy => new VeryBusyExpAnalysisSimpleSolver(cfg) <----- Complete here
          //case Analysis.reaching => new ReachingDefAnalysisSimpleSolver(cfg) <----- Complete here
          case Analysis.constprop => new ConstantPropagationAnalysisSimpleSolver(cfg)
          case _ => throw new RuntimeException(s"Unsupported option for the analysis $kind")
        })
      case AnalysisOption.`wl` =>
        Some(kind match {
          case Analysis.sign => new IntraprocSignAnalysisWorklistSolver(cfg)
          case Analysis.livevars => new LiveVarsAnalysisWorklistSolver(cfg)
          case Analysis.available => new AvailableExpAnalysisWorklistSolver(cfg)
          //case Analysis.vbusy => new VeryBusyExpAnalysisWorklistSolver(cfg) <----- Complete here
          //case Analysis.reaching => new ReachingDefAnalysisWorklistSolver(cfg) <----- Complete here
          case Analysis.constprop => new ConstantPropagationAnalysisWorklistSolver(cfg)
          case _ => throw new RuntimeException(s"Unsupported option for the analysis $kind")
        })
      case AnalysisOption.`wli` =>
        Some(kind match {
          case Analysis.sign => new IntraprocSignAnalysisWorklistSolverWithInit(cfg)
          case _ => throw new RuntimeException(s"Unsupported option for the analysis $kind")
        })
      case AnalysisOption.`wliw` =>
        Some(kind match {
          case Analysis.interval => new IntervalAnalysisWorklistSolverWithWidening(cfg)
          case _ => throw new RuntimeException(s"Unsupported option for the analysis $kind")
        })
      case AnalysisOption.`wliwn` =>
        Some(kind match {
          case Analysis.interval => new IntervalAnalysisWorklistSolverWithWideningAndNarrowing(cfg)
          case _ => throw new RuntimeException(s"Unsupported option for the analysis $kind")
        })
      case AnalysisOption.`wlip` =>
        Some(kind match {
          case Analysis.sign => new IntraprocSignAnalysisWorklistSolverWithInitAndPropagation(cfg)
          case _ => throw new RuntimeException(s"Unsupported option for the analysis $kind")
        })
      case AnalysisOption.`iwli` =>
        
        val wcfg = cfg match {
          case w: ProgramCFG => w
          case _ => throw new RuntimeException(s"Whole CFG needed")
        }
        
        Some(kind match {
          case Analysis.sign => new InterprocSignAnalysisWorklistSolverWithInit(wcfg)
          case _ => throw new RuntimeException(s"Unsupported option for the analysis $kind")
        })
      case AnalysisOption.`iwlip` =>
        
        val wcfg = cfg match {
          case w: ProgramCFG => w
          case _ => throw new RuntimeException(s"Whole CFG needed")
        }
        
        Some(kind match {
          case Analysis.sign => new InterprocSignAnalysisWorklistSolverWithInitAndPropagation(wcfg)
          case _ => throw new RuntimeException(s"Unsupported option for the analysis $kind")
        })
      case AnalysisOption.`iwlic` =>
        
        val wcfg = cfg match {
          case w: ProgramCFG => w
          case _ => throw new RuntimeException(s"Whole cfg needed")
        }
        
        Some(kind match {
          case Analysis.sign => new InterprocSignAnalysisWorklistSolverWithInitAndCFA(wcfg)
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
   * - iwlic: tries to create an analysis which uses the worklist solver with init, interprocedural version with CFA analysis
   */
  object AnalysisOption extends Enumeration {
    val simple, Disabled, wl, wli, wliw, wliwn, wlip, iwli, iwlip, iwlic = Value
    
    def intraprocedural(v: Value): Boolean = {
      v match {
        case `iwli` => true
        case `iwlip` => true
        case `iwlic` => true
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
