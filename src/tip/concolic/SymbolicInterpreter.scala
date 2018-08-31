package tip.concolic

import tip.ast._
import tip.ast.AstNodeData.DeclarationData
import tip.interpreter.Interpreter
import SMTSolver.Symbol

abstract class SymbolicInterpreter(program: AProgram)(implicit declData: DeclarationData) extends Interpreter(program) {

  val spec = SymbolicValues

  import spec._

  case class ConcolicState(
    var symbols: List[Symbol] = Nil,
    var ct: ExecutionTree,
    var counter: Int = 0,
    var inputs: List[Int] = Nil,
    var usedInputs: List[Int] = Nil
  )

  var cState: ConcolicState

  override protected def semc(stm: AStmt, env: Env, store: Store): (Env, Store) =
    stm match {
      case AIfStmt(guard, ifBranch, elseBranch, loc) =>
        val (gv, s1) = semeright(guard, env, store)
        gv match {
          case x: SymbIntValue if spec.eqqInt(x, spec.constInt(0)) =>
            cState.ct = cState.ct.branch(guard, x.symbolic, false)
            elseBranch.map(stmt => semc(stmt, env, s1)).getOrElse((env, s1))
          case x: SymbIntValue if spec.eqqInt(x, spec.constInt(1)) =>
            cState.ct = cState.ct.branch(guard, x.symbolic, true)
            semc(ifBranch, env, s1)
          case _ => errorConditionNotInt(loc)
        }
      case err: AErrorStmt =>
        val (ev, _) = semeright(err.value, env, store)
        ev match {
          case e: SymbIntValue =>
            throw SymbolicInterpreterException(e.i, s"Application exception occurred during program execution, error code: ${e.i}")
          case _ => errorErrorNonInt(ev)
        }
      case w: AWhileStmt =>
        val (gv, s1) = semeright(w.guard, env, store)
        gv match {
          case x: SymbIntValue if spec.eqqInt(x, spec.constInt(0)) =>
            cState.ct = cState.ct.branch(w.guard, x.symbolic, false)
            (env, s1)
          case x: SymbIntValue =>
            cState.ct = cState.ct.branch(w.guard, x.symbolic, true)
            val (env1, s2) = semc(w.innerBlock, env, s1)
            semc(w, env1, s2)
          case _ => errorConditionNotInt(w.loc)
        }
      case _ => super.semc(stm, env, store)
    }

  override protected def semeright(exp: AExpr, env: Env, store: Store): (EValue, Store) =
    exp match {
      case AInput(_) =>
        val newSymbol = new Symbol(exp.loc, cState.counter)
        val num = if (cState.counter < cState.inputs.length) {
          cState.inputs(cState.counter)
        } else {
          scala.util.Random.nextInt
        }
        cState.counter += 1
        cState.symbols = cState.symbols ::: List(newSymbol)
        cState.usedInputs = cState.usedInputs ::: List(num)
        val v = SymbIntValue(num, newSymbol)
        log.info(s"Generated symbolic input value: $v.")
        (v, store)
      case _ => super.semeright(exp, env, store)
    }

  case class SymbolicInterpreterException(code: Int, message: String) extends RuntimeException(message)

}
