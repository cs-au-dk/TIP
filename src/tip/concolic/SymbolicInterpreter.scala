package tip.concolic

import tip.ast._
import tip.ast.AstNodeData.DeclarationData
import tip.interpreter.Interpreter
import SMTSolver.Symbol

abstract class SymbolicInterpreter(program: AProgram)(implicit declData: DeclarationData) extends Interpreter(program) {
  val spec = SymbolicValues
  import spec._
  type Extra = ConcolicState

  case class ConcolicState(symbols: List[Symbol] = Nil, ct: ExecutionTree, counter: Int = 0, inputs: List[Int], usedInputs: List[Int] = Nil) {
    def usedInput(symbol: Symbol, num: Int): ConcolicState =
      this.copy(symbols = symbols :+ symbol, usedInputs = usedInputs :+ num, counter = counter + 1)
    def getInput: Int =
      if (counter < inputs.length) {
        inputs(counter)
      } else {
        scala.util.Random.nextInt
      }
  }

  def semp(ct: ExecutionTree, inputs: List[Int]): (IntValue, Store) = {
    val initEnv: Env = Map()
    val initStore: Store = Store(Map(), ConcolicState(ct = ct, inputs = inputs))
    semp(initEnv, initStore)
  }

  def semp(): IntValue =
    semp(new ExecutionTreeRoot(), Nil)._1

  override protected def branchTaken(guard: AExpr, value: EValue, branch: Boolean, store: Store): Store =
    value match {
      case x: SymbIntValue =>
        store.setExtra(store.extra.copy(ct = store.extra.ct.branch(guard, x.symbolic, branch)))
      case _ => store
    }

  override protected def input(exp: AExpr, env: Env, store: Store): (EValue, Store) = {
    val newSymbol = new Symbol(exp.loc, store.extra.counter)
    val num = store.extra.getInput
    val v = SymbIntValue(num, newSymbol)
    log.info(s"Generated symbolic input value: $v.")
    (v, store.setExtra(store.extra.usedInput(newSymbol, num)))
  }
}
