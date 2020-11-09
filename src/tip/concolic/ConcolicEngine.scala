package tip.concolic

import tip.ast.AstNodeData._
import tip.ast.AProgram
import tip.util.Log
import SMTSolver.Symbol

class ConcolicEngine(val program: AProgram)(implicit declData: DeclarationData) extends SymbolicInterpreter(program) {

  override val log = Log.logger[this.type]()

  def nextExplorationTarget(lastExplored: ExecutionTree, root: ExecutionTreeRoot): Option[(Branch, Boolean)] =
    lastExplored.parent match {
      case b: Branch =>
        (b.branches(true), b.branches(false)) match {
          case (_: SubTreePlaceholder, _) if b.count(true) == 0 => Some((b, true))
          case (_, _: SubTreePlaceholder) if b.count(false) == 0 => Some((b, false))
          case (_, _) => nextExplorationTarget(b, root)
        }
      case _ => None
    }

  def newInputs(symbols: List[Symbol], lastNode: ExecutionTree, root: ExecutionTreeRoot): Option[List[Int]] = {
    if (lastNode == root) {
      log.info("Program never branches")
      return None
    }
    val target = nextExplorationTarget(lastNode, root)
    log.info(s"Execution tree status: \n${ExecutionTreePrinter.printExecutionTree(root)}")
    target match {
      case Some((targetNode, value)) =>
        val pc = targetNode.pathCondition(List((targetNode.symcondition, value)))
        log.info(s"Path condition for next run: $pc")
        val smt = SMTSolver.pathToSMT(symbols, pc)
        log.info(s"SMT script for next run: \n$smt")
        SMTSolver.solve(smt) match {
          case None =>
            log.info(s"Path condition is unsatisfiable")
            targetNode.unsat(value)
            newInputs(symbols, lastNode, root)
          case Some(mapping) =>
            log.info(s"Model: $mapping")
            Some(symbols.map(v => mapping.get(v.name).map(_.toInt).getOrElse(scala.util.Random.nextInt)))
        }
      case _ => None
    }
  }

  def test(budget: Int = 20): Unit = {
    val root = new ExecutionTreeRoot()

    var runs = 0
    var inputs: List[Int] = Nil
    var results: List[ExecutionResult] = Nil
    while (runs <= budget) {

      runs += 1

      log.info("\n")
      log.info(s"Starting run $runs")

      val result: ExecutionResult =
        try {
          semp(root, inputs) match {
            case (spec.SymbIntValue(i, _), store) =>
              log.info(s"Program ran successfully, result: $i")
              ExSuccess(store.extra, i)
            case _ => ???
          }
        } catch {
          case err: ExecutionError =>
            log.info(s"Error found: $err")
            ExFailure(err.store.extra, err.getMessage)
        }

      results = result :: results
      newInputs(result.symbolicVars, result.lastNode, root) match {
        case Some(values) =>
          log.info(s"New input for ${result.symbolicVars}: $values")
          inputs = values
        case None =>
          log.info(s"Finished exhaustive exploration in $runs runs")
          reportExplorationStatistics(results)
          return
      }
    }
    log.info(s"Exhausted search budget after $runs runs")
    reportExplorationStatistics(results)
  }

  private def reportExplorationStatistics(results: List[ExecutionResult]): Unit = {
    val successes = results.collect { case s: ExSuccess => s }
    log.info(s"Found ${successes.length} successful input sequences")
    successes.foreach(s => log.info(s"Input sequence ${s.usedInputs} produces: \n${s.value}"))
    val failures = results.collect { case f: ExFailure => f }
    log.info(s"Found ${failures.length} failure-inducing input sequences.")
    failures.foreach(f => log.info(s"Input sequence ${f.usedInputs} fails: \n${f.message}."))
  }

  abstract class ExecutionResult(val concolicState: ConcolicState) {
    val lastNode = concolicState.ct
    val symbolicVars = concolicState.symbols
    val usedInputs = concolicState.usedInputs
  }

  case class ExSuccess(s: ConcolicState, value: Int) extends ExecutionResult(s)

  case class ExFailure(s: ConcolicState, message: String) extends ExecutionResult(s)

}
