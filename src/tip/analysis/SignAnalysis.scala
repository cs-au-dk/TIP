package tip.analysis

import tip.ast.AstNodeData.DeclarationData
import tip.cfg.{InterproceduralProgramCfg, IntraproceduralProgramCfg}
import tip.lattices.SignLattice

object SignAnalysis {

  object Intraprocedural {

    /**
      * Intraprocedural analysis that uses the simple fixpoint solver.
      */
    class SimpleSolver(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData) extends IntraprocValueAnalysisSimpleSolver(cfg, SignLattice)

    /**
      * Intraprocedural analysis that uses the worklist solver.
      */
    class WorklistSolver(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData) extends IntraprocValueAnalysisWorklistSolver(cfg, SignLattice)

    /**
      * Intraprocedural analysis that uses the worklist solver with reachability.
      */
    class WorklistSolverWithReachability(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
        extends IntraprocValueAnalysisWorklistSolverWithReachability(cfg, SignLattice)

    /**
      * Intraprocedural analysis that uses the worklist solver with reachability and propagation-style.
      */
    class WorklistSolverWithReachabilityAndPropagation(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
        extends IntraprocValueAnalysisWorklistSolverWithReachabilityAndPropagation(cfg, SignLattice)
  }

  object Interprocedural {

    /**
      * Interprocedural analysis that uses the worklist solver with reachability.
      */
    class WorklistSolverWithReachability(cfg: InterproceduralProgramCfg)(implicit declData: DeclarationData)
        extends InterprocValueAnalysisWorklistSolverWithReachability(cfg, SignLattice)

    /**
      * Interprocedural analysis that uses the worklist solver with reachability and propagation-style.
      */
    class WorklistSolverWithReachabilityAndPropagation(cfg: InterproceduralProgramCfg)(implicit declData: DeclarationData)
        extends InterprocValueAnalysisWorklistSolverWithReachabilityAndPropagation(cfg, SignLattice)

    /**
      * Interprocedural analysis that uses the worklist solver with reachability and propagation-style.
      * with call-string context sensitivity.
      */
    class CallString(cfg: InterproceduralProgramCfg)(implicit declData: DeclarationData) extends CallStringValueAnalysis(cfg, SignLattice)

    /**
      * Interprocedural analysis that uses the worklist solver with reachability and propagation-style.
      * with functional-approach context sensitivity.
      */
    class Functional(cfg: InterproceduralProgramCfg)(implicit declData: DeclarationData) extends FunctionalValueAnalysis(cfg, SignLattice)
  }
}
