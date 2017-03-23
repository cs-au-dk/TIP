package tip.interpreter

import tip.ast.AstNodeData._
import tip.ast._
import tip.util.Log

/**
  * Interpreter for TIP programs.
  */
abstract class Interpreter(program: AProgram)(implicit declData: DeclarationData) {

  val spec: ValueSpecification // specification of values and operations used by the interpreter

  val log = Log.logger[this.type]()

  import spec._
  type Env = Map[ADeclaration, Location] // environments map from identifier declarations to locations
  type Store = Map[ReferenceValue, Value] // stores map from locations to values

  /**
    * Semantics for programs.
    * @return the resulting value
    */
  def semp(): IntValue = {
    // Make an initial environment with a location for each function
    val boundEnv = program.funs.foldLeft(Map(): Env) { (a: Env, f: AFunDeclaration) =>
      a + (f -> newLoc())
    }
    // Store the functions in the associated locations
    val boundStore = program.funs.foldLeft(Map(): Store) { (s: Store, f: AFunDeclaration) =>
      s + (boundEnv(f) -> spec.mkFun(f))
    }
    // Execute the main function
    val (_, cs) = semc(program.mainFunction.stmts, boundEnv, boundStore)
    // Return the result, if type int
    cs(returnLoc) match {
      case x: IntValue => x
      case _ => errorReturnNotInt(program.mainFunction)
    }
  }

  /**
    * Semantics for functions.
    * @param f the function to execute
    * @param actualParams actual parameters
    * @param env the initial environment (containing function names)
    * @param store the initial store
    * @return the resulting store
    */
  private def semf(f: AFunDeclaration, actualParams: List[EValue], env: Env, store: Store): Store = {
    // Extend the environment with ...
    val extEnv: Env = (
      // ... the formal parameters
      f.args.map { id =>
        id -> newLoc()
      }
        ++
          // ... and the local variables
          f.stmts.declarations.flatMap { vs =>
            vs.declIds.map { v =>
              v -> newLoc()
            }
          }
    ).toMap
    val nEnv = env ++ extEnv
    // Write the actual parameters to the corresponding locations in the store
    val nStore = f.args.zip(actualParams).foldLeft(store) { (ns: Store, p: (AIdentifierDeclaration, EValue)) =>
      ns + (extEnv(p._1) -> p._2)
    }
    // Execute the body
    val (_, finalStore) = semc(f.stmts, nEnv, nStore)
    // Remove the formal parameters and local variables from the store
    finalStore -- extEnv.values
  }

  /**
    * Semantics for statements (including local variable declarations).
    * @param stm the statement to execute
    * @param env the initial environment
    * @param store the initial store
    * @return the resulting environment and store
    */
  protected def semc(stm: AStmt, env: Env, store: Store): (Env, Store) = {
    stm match {
      case AAssignStmt(left, right: AExpr, _) =>
        val (lv, s1) = semeref(left.fold(identity, identity), env, store)
        val (rv, s2) = semeright(right, env, s1)
        (env, s2 + (lv -> rv))
      case block: ABlock =>
        block.body.foldLeft((env, store))((acc: (Env, Store), stm: AStmt) => semc(stm, acc._1, acc._2))
      case AIfStmt(guard, ifBranch, elseBranch, loc) =>
        val (gv, s1) = semeright(guard, env, store)
        gv match {
          case x: IntValue if spec.eqqInt(x, spec.constInt(0)) =>
            elseBranch.map(stmt => semc(stmt, env, s1)).getOrElse((env, s1))
          case _: IntValue =>
            semc(ifBranch, env, s1)
          case _ => errorConditionNotInt(loc)
        }
      case ret: AReturnStmt =>
        val (v, s1) = semeright(ret.value, env, store)
        (env, s1 + (returnLoc -> v))
      case err: AErrorStmt =>
        val (ev, _) = semeright(err.value, env, store)
        ev match {
          case i: IntValue => throw ExecutionError(i)
          case _ => errorErrorNonInt(ev)
        }
      case w: AWhileStmt =>
        val (gv, s1) = semeright(w.guard, env, store)
        gv match {
          case x: IntValue if spec.eqqInt(x, spec.constInt(0)) => (env, s1)
          case _: IntValue =>
            val (env1, s2) = semc(w.innerBlock, env, s1)
            semc(w, env1, s2)
          case _ => errorConditionNotInt(w.loc)
        }
      case AOutputStmt(value, _) =>
        val (ov, s1) = semeright(value, env, store)
        ov match {
          case y: IntValue =>
            log.info(s"Program out: ${y.i}")
            (env, s1)
          case _ => errorOutputNotInt()
        }
      case AVarStmt(ids, _) =>
        (ids.foldLeft(env) { (accenv, id) =>
          accenv + (id -> newLoc())
        }, store)
    }
  }

  /**
    * Semantics for left-hand-side expressions.
    * @param exp the expression to execute
    * @param env the environment
    * @param store the initial store
    * @return the resulting location and store
    */
  protected def semeref(exp: AExpr, env: Env, store: Store): (ReferenceValue, Store) = {
    exp match {
      case id: AIdentifier => (env(id.declaration), store)
      case AUnaryOp(_: DerefOp.type, target, loc) =>
        semeright(target, env, store) match {
          case (pref: ReferenceValue, s1) => (pref, s1)
          case (_: NullValue, _) => errorNullDereference(loc)
          case (x, _) => errorDerefNotPointer(loc, store, x)
        }
      case _ => ???
    }
  }

  /**
    * Semantics for right-hand-side expressions.
    * @param exp the expression to execute
    * @param env the environment
    * @param store the initial store
    * @return the resulting value and store
    */
  protected def semeright(exp: AExpr, env: Env, store: Store): (EValue, Store) = {
    exp match {
      case e: ABinaryOp =>
        val (left: EValue, s1) = semeright(e.left, env, store)
        val (right: EValue, s2) = semeright(e.right, env, s1)
        val cval = e.operator match {
          case Eqq => spec.eqq(left, right)
          case op: BinaryOperator =>
            (left, right) match {
              case (lv: IntValue, rv: IntValue) =>
                op match {
                  case Divide => spec.divideInt(lv, rv)
                  case GreatThan => spec.greatThanInt(lv, rv)
                  case Minus => spec.minusInt(lv, rv)
                  case Plus => spec.plusInt(lv, rv)
                  case Times => spec.timesInt(lv, rv)
                  case _ => ???
                }
              case _ => errorArithmeticOnNonInt(op)
            }
        }
        (cval, s2)
      case AInput(_) =>
        val line = scala.io.StdIn.readLine()
        val cval = if (line == null) spec.constInt(0) else spec.constInt(line.toInt)
        (cval, store)
      case AMalloc(_) => (newLoc(), store)
      case ANull(_) => (spec.nullValue, store)
      case ANumber(value, _) => (spec.constInt(value), store)
      case AUnaryOp(_: RefOp.type, e: AExpr, _) =>
        semeref(e, env, store)
      case op @ AUnaryOp(_: DerefOp.type, _, _) =>
        val (l, s1) = semeref(op, env, store)
        (s1.getOrElse(l, errorDerefNotPointer(exp.loc, s1, l)), s1)
      case ACallFuncExpr(target, actualParams, _) =>
        val (funValue, s1) = semeright(target, env, store)
        funValue match {
          case f: FunValue =>
            val (actualParamsValues, computedStore) =
              actualParams.foldRight((List[EValue](), s1)) { (arg: AExpr, p: (List[EValue], Store)) =>
                val (computed, s1) = p
                val (v, s2) = semeright(arg, env, s1)
                (v :: computed, s2)
              }
            val finStore = semf(f.fun, actualParamsValues, env, computedStore)
            (finStore(returnLoc), finStore)
          case _ => errorCallNotFunction(funValue)
        }
      case id: AIdentifier =>
        val (l, s1) = semeref(id, env, store)
        (s1.getOrElse(l, errorDerefNotPointer(exp.loc, s1, l)), s1)
    }
  }

  def errorCallNotFunction(funValue: EValue) =
    throw new RuntimeException(s"Call to a non-function $funValue")
  def errorBadLeftHand(x: Any) =
    throw new RuntimeException(s"Bad left-hand-side $x of assignment")
  def errorOutputNotInt() =
    throw new RuntimeException(s"Output not supported for non-integer values")
  def errorErrorNonInt(v: Any) =
    throw new RuntimeException(s"Error statement expects integer value as error code, given $v")
  def errorNonRefable(exp: AExpr) =
    throw new RuntimeException(s"Cannot take the address of an expression at $exp")
  def errorArithmeticOnNonInt(op: BinaryOperator) =
    throw new RuntimeException(s"Unable to apply the operator $op to non-integer values")
  def errorReturnNotInt(fun: AFunDeclaration) =
    throw new RuntimeException(s"Return statement returning non-integer in function ${fun.name}")
  def errorNullDereference(loc: Loc) =
    throw new RuntimeException(s"Null pointer error at $loc")
  def errorConditionNotInt(loc: Loc) =
    throw new RuntimeException(s"Branch condition at $loc not evaluating to an integer")
  def errorDerefNotPointer(loc: Loc, store: Store, x: Any) =
    throw new RuntimeException(s"Dereferencing non or bad pointer at $loc: $x in $store")

  case class ExecutionError(code: IntValue) extends RuntimeException(s"Execution error, code: $code")
}

/**
  * Interpreter that uses concrete values.
  */
class ConcreteInterpreter(val program: AProgram)(implicit declData: DeclarationData) extends Interpreter(program) {
  val spec = ConcreteValues
}
