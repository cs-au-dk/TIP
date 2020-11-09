package tip.interpreter

import tip.ast.AstNodeData._
import tip.ast._
import tip.util.{Log, TipProgramException}

import scala.util.{Failure, Success, Try}

/**
  * Interpreter for TIP programs.
  */
abstract class Interpreter(program: AProgram)(implicit declData: DeclarationData) {

  val spec: ValueSpecification // specification of values and operations used by the interpreter

  val log = Log.logger[this.type]()

  import spec._

  type Env = Map[ADeclaration, Location] // environments map from identifier declarations to locations
  type Extra // extra content to keep in the store
  // stores map from locations to values and has possible extra content
  case class Store(content: Map[ReferenceValue, Value], extra: Extra) {
    def get(key: ReferenceValue) = content.get(key)
    def +(kv: (ReferenceValue, Value)) = this.copy(content = content + kv)
    def -(key: ReferenceValue) = this.copy(content = content - key)
    def keys = content.keys
    def values = content.values

    def apply(key: ReferenceValue): Value = content(key)
    def getOrElse(key: ReferenceValue, default: => Value): Value = get(key).getOrElse(default)
    def ++(xs: Iterable[(ReferenceValue, Value)]): Store =
      xs.seq.foldLeft(this)((acc: Store, kv: (ReferenceValue, Value)) => acc + kv)
    def --(xs: Iterable[ReferenceValue]): Store =
      xs.seq.foldLeft(this)((acc: Store, key: ReferenceValue) => acc - key)

    def setExtra(extra: Extra): Store = this.copy(extra = extra)
  }

  val capturedOut: StringBuilder = StringBuilder.newBuilder

  /**
    * Semantics for programs.
    *
    * @return the resulting value
    */
  def semp(initEnv: Env, initStore: Store): (IntValue, Store) = {
    capturedOut.clear()
    // Make an initial environment with a location for each function
    val boundEnv = program.funs.foldLeft(initEnv) { (a: Env, f: AFunDeclaration) =>
      a + (f -> newLoc())
    }
    // Create a location for each formal argument of main
    val envWithInputArgs = program.mainFunction.params.foldLeft(boundEnv) { (a: Env, id: AIdentifierDeclaration) =>
      a + (id -> newLoc())
    }
    // Store the functions in the associated locations
    val boundStore = program.funs.foldLeft(initStore) { (s: Store, f: AFunDeclaration) =>
      s + (boundEnv(f) -> spec.mkFun(f))
    }
    // Store the input in the associated argument locations
    val storeWithInputArgs = program.mainFunction.params.foldLeft(boundStore) { (s: Store, id: AIdentifierDeclaration) =>
      val (v, s2) = input(AIdentifier(id.name, id.loc), boundEnv, s)
      s2 + (envWithInputArgs(id) -> v)
    }
    // Execute the main function
    val (_, cs) = semc(program.mainFunction.stmts, envWithInputArgs, storeWithInputArgs)
    // Return the result, if type int
    cs(returnLoc) match {
      case x: IntValue =>
        output(x)
        (x, cs)
      case _ =>
        errorReturnNotInt(program.mainFunction.stmts.ret.exp.loc, program.mainFunction, cs)
    }
  }

  /**
    * Semantics for functions.
    *
    * @param f            the function to execute
    * @param actualParams actual parameters
    * @param env          the initial environment (containing function names)
    * @param store        the initial store
    * @return the resulting store
    */
  private def semf(f: AFunDeclaration, actualParams: List[EValue], env: Env, store: Store): Store = {
    // Extend the environment with ...
    val extEnv = (
      // ... the formal parameters
      f.params.map { id =>
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
    val nStore = f.params.zip(actualParams).foldLeft(store) { (ns: Store, p: (AIdentifierDeclaration, EValue)) =>
      ns + (extEnv(p._1) -> p._2)
    }
    // Execute the body
    val (_, finalStore) = semc(f.stmts, nEnv, nStore)
    // Remove the formal parameters and local variables from the store
    finalStore -- extEnv.values
  }

  protected def branchTaken(guard: AExpr, value: EValue, branch: Boolean, store: Store): Store =
    store

  /**
    * Semantics for statements (including local variable declarations).
    *
    * @param stm   the statement to execute
    * @param env   the initial environment
    * @param store the initial store
    * @return the resulting environment and store
    */
  protected def semc(stm: AStmt, env: Env, store: Store): (Env, Store) =
    stm match {
      case AAssignStmt(left: FieldAssignable, right: AExpr, _) =>
        val (lv, field, s1) = semeref(left, env, store)
        store.getOrElse(lv, errorUninitializedLocation(stm.loc, s1)) match {
          case rec: RecordValue =>
            val (rv, s2) = semeright(right, env, s1)
            if (rv.isInstanceOf[RecordValue]) errorWriteFieldRecord(stm.loc, rv, s2)
            (env, s2 + (lv -> mkRecord(rec.fields + (field -> rv))))
          case _ =>
            errorAccessNonRecord(stm.loc, lv, s1)
        }
      case AAssignStmt(left: ReferenceAssignable, right: AExpr, _) =>
        val (lv, s1) = semeref(left, env, store)
        val (rv, s2) = semeright(right, env, s1)
        (env, s2 + (lv -> rv))
      case block: ABlock =>
        block.body.foldLeft((env, store))((acc: (Env, Store), stm: AStmt) => semc(stm, acc._1, acc._2))
      case AIfStmt(guard, ifBranch, elseBranch, loc) =>
        val (gv, s1) = semeright(guard, env, store)
        gv match {
          case x: IntValue if spec.eqqInt(x, spec.constInt(0)) =>
            val s2 = branchTaken(guard, gv, false, store)
            elseBranch.map(stmt => semc(stmt, env, s2)).getOrElse((env, s2))
          case _: IntValue =>
            val s2 = branchTaken(guard, gv, true, store)
            semc(ifBranch, env, s2)
          case _ => errorConditionNotInt(loc, s1)
        }
      case ret: AReturnStmt =>
        val (v, s1) = semeright(ret.exp, env, store)
        (env, s1 + (returnLoc -> v))
      case err: AErrorStmt =>
        val (ev, s1) = semeright(err.exp, env, store)
        ev match {
          case i: IntValue => throw new ExecutionError(s"Execution error, code: ${i.i}", s1)
          case _ => errorErrorNonInt(stm.loc, ev, store)
        }
      case w: AWhileStmt =>
        val (gv, s1) = semeright(w.guard, env, store)
        gv match {
          case x: IntValue if spec.eqqInt(x, spec.constInt(0)) =>
            val s2 = branchTaken(w.guard, gv, false, store)
            (env, s2)
          case _: IntValue =>
            val s2 = branchTaken(w.guard, gv, true, store)
            val (env1, s3) = semc(w.innerBlock, env, s2)
            semc(w, env1, s3)
          case _ => errorConditionNotInt(w.loc, s1)
        }
      case AOutputStmt(exp, _) =>
        val (ov, s1) = semeright(exp, env, store)
        ov match {
          case y: IntValue =>
            output(y)
            (env, s1)
          case y: ReferenceValue =>
            output(y)
            (env, s1)
          case _ => errorOutputNotInt(exp.loc, s1)
        }
      case AVarStmt(ids, _) =>
        (ids.foldLeft(env) { (accenv, id) =>
          accenv + (id -> newLoc())
        }, store)
    }

  /**
    * Semantics for ordinary lvalue expressions.
    *
    * @param exp   the lvalue expression to execute
    * @param env   the environment
    * @param store the initial store
    * @return the resulting location and store
    */
  protected def semeref(exp: ReferenceAssignable, env: Env, store: Store): (ReferenceValue, Store) =
    exp match {
      case id: AIdentifier => (env(id.declaration), store)
      case ADerefWrite(subexp, loc) =>
        semeright(subexp, env, store) match {
          case (pref: ReferenceValue, s1) => (pref, s1)
          case (_: NullValue, s1) => errorNullDereference(loc, s1)
          case (x, s1) => errorDerefNotPointer(loc, x, s1)
        }
      case _ => ???
    }

  /**
    * Semantics for field lvalue expressions.
    *
    * @param exp   the lvalue expression to execute
    * @param env   the environment
    * @param store the initial store
    * @return the resulting location, field, and store
    */
  protected def semeref(exp: FieldAssignable, env: Env, store: Store): (ReferenceValue, String, Store) =
    exp match {
      case ADirectFieldWrite(id, field, _) => (env(id.declaration), field, store)
      case AIndirectFieldWrite(fexp, field, loc) =>
        semeright(fexp, env, store) match {
          case (pref: ReferenceValue, s1) => (pref, field, s1)
          case (_: NullValue, s1) => errorNullDereference(loc, s1)
          case (x, s1) => errorDerefNotPointer(loc, x, s1)
        }
    }

  /**
    * Semantics for right-hand-side expressions.
    *
    * @param exp the expression to execute
    * @param env the environment
    * @param store the initial store
    * @return the resulting value and store
    */
  protected def semeright(exp: AExpr, env: Env, store: Store): (EValue, Store) =
    exp match {
      case access: AFieldAccess =>
        val (tv, s1) = semeright(access.record, env, store)
        tv match {
          case rec: RecordValue =>
            if (rec.fields.contains(access.field)) {
              (rec.fields(access.field), s1)
            } else {
              errorAccessMissingField(access.loc, rec, access.field, s1)
            }
          case _ =>
            errorAccessNonRecord(access.loc, tv, s1)
        }
      case record: ARecord =>
        val (fields, s2) = record.fields.foldLeft((Map[String, EValue](), store)) {
          case ((m, s), f) =>
            val (v, s1) = semeright(f.exp, env, s)
            (m + (f.field -> v), s1)
        }
        (mkRecord(fields), s2)
      case e: ABinaryOp =>
        val (left: EValue, s1) = semeright(e.left, env, store)
        val (right: EValue, s2) = semeright(e.right, env, s1)
        val cval = e.operator match {
          case Eqq => spec.eqq(left, right)
          case op: BinaryOperator =>
            (left, right) match {
              case (lv: IntValue, rv: IntValue) =>
                op match {
                  case Eqq => spec.eqq(lv, rv)
                  case Divide => spec.divideInt(lv, rv)
                  case GreatThan => spec.greatThanInt(lv, rv)
                  case Minus => spec.minusInt(lv, rv)
                  case Plus => spec.plusInt(lv, rv)
                  case Times => spec.timesInt(lv, rv)
                }
              case _ => errorArithmeticOnNonInt(exp.loc, op, s2)
            }
        }
        (cval, s2)
      case e: AUnaryOp =>
        val (sub, s1) = semeright(e.subexp, env, store)
        val cval = e.operator match {
          case DerefOp =>
            sub match {
              case crv: ReferenceValue =>
                s1.getOrElse(crv, errorUninitializedLocation(exp.loc, s1))
              case _ => errorDerefNotPointer(exp.loc, sub, s1)
            }
        }
        (cval, s1)
      case AInput(_) =>
        input(exp, env, store)
      case AAlloc(content, _) =>
        val (cv, s1) = semeright(content, env, store)
        val l = newLoc()
        (l, s1 + (l -> cv))
      case ANull(_) => (spec.nullValue, store)
      case ANumber(value, _) => (spec.constInt(value), store)
      case AVarRef(e: AIdentifier, _) =>
        (env(e.declaration), store)
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
          case _ => errorCallNotFunction(target.loc, funValue, s1)
        }
      case id: AIdentifier =>
        val l = env(id.declaration)
        (store.getOrElse(l, errorUninitializedLocation(exp.loc, store)), store)
    }

  /**
    * Output `y` to stdout.
    */
  private def output(v: Value): Unit =
    v match {
      case i: IntValue =>
        println(s"Program output: ${i.i}")
      case r: ReferenceValue =>
        println(s"Program output: $r")
    }

  /**
    * Takes an integer input from stdin.
    */
  protected def input(exp: AExpr, env: Env, store: Store): (EValue, Store) = {
    print(s"Enter input: ")
    Console.flush()
    val line = scala.io.StdIn.readLine()
    if (line == null) {
      (spec.constInt(0), store)
    } else {
      Try(line.toInt) match {
        case Success(i) => (spec.constInt(i), store)
        case Failure(_) => errorInputNotInt(exp.loc, store)
      }
    }
  }

  class ExecutionError(message: String, val store: Store) extends TipProgramException(s"Runtime error: $message")

  def errorCallNotFunction(loc: Loc, funValue: EValue, store: Store) =
    throw new ExecutionError(s"Call to a non-function $funValue ${loc.toStringLong}", store)

  def errorOutputNotInt(loc: Loc, store: Store) =
    throw new ExecutionError(s"Output not supported for non-integer values ${loc.toStringLong}", store)

  def errorInputNotInt(loc: Loc, store: Store) =
    throw new ExecutionError(s"Input not supported for non-integer values ${loc.toStringLong}", store)

  def errorErrorNonInt(loc: Loc, v: EValue, store: Store) =
    throw new ExecutionError(s"Error statement non-integer value $v ${loc.toStringLong}", store)

  def errorArithmeticOnNonInt(loc: Loc, op: BinaryOperator, store: Store) =
    throw new ExecutionError(s"Unable to apply the operator $op to non-integer value ${loc.toStringLong}", store)

  def errorReturnNotInt(loc: Loc, fun: AFunDeclaration, store: Store) =
    throw new ExecutionError(s"Return statement returning non-integer in function ${fun.name} ${loc.toStringLong}", store)

  def errorNullDereference(loc: Loc, store: Store) =
    throw new ExecutionError(s"Null pointer dereference ${loc.toStringLong}", store)

  def errorConditionNotInt(loc: Loc, store: Store) =
    throw new ExecutionError(s"Branch condition not evaluating to an integer ${loc.toStringLong}", store)

  def errorDerefNotPointer(loc: Loc, x: EValue, store: Store) =
    throw new ExecutionError(s"Dereferencing non-pointer $x ${loc.toStringLong}", store)

  def errorAccessNonRecord(loc: Loc, x: EValue, store: Store) =
    throw new ExecutionError(s"Accessing field on non-record value $x ${loc.toStringLong}", store)

  def errorWriteFieldRecord(loc: Loc, x: EValue, store: Store) =
    throw new ExecutionError(s"Writing record to field $x ${loc.toStringLong}", store)

  def errorAccessMissingField(loc: Loc, rec: RecordValue, field: String, store: Store) =
    throw new ExecutionError(s"Accessing missing field $field in record $rec ${loc.toStringLong}", store)

  def errorUninitializedLocation(loc: Loc, store: Store) =
    throw new ExecutionError(s"Accessing uninitialized memory location ${loc.toStringLong}", store)
}

/**
  * Interpreter that uses concrete values.
  */
class ConcreteInterpreter(val program: AProgram)(implicit declData: DeclarationData) extends Interpreter(program) {
  val spec = ConcreteValues
  type Extra = Unit
  def semp(): spec.IntValue = semp(Map(), Store(Map(), ()))._1
}
