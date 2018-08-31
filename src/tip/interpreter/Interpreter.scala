package tip.interpreter

import tip.ast.AstNodeData._
import tip.ast._
import tip.util.Log

import scala.util.{Failure, Success, Try}

/**
  * Interpreter for TIP programs.
  */
abstract class Interpreter(program: AProgram)(implicit declData: DeclarationData) {

  val spec: ValueSpecification // specification of values and operations used by the interpreter

  val log = Log.logger[this.type]()

  import spec._

  type Env = Map[ADeclaration, Location] // environments map from identifier declarations to locations
  type Store = Map[ReferenceValue, Value] // stores map from locations to values

  val capturedOut: StringBuilder = StringBuilder.newBuilder

  /**
    * Semantics for programs.
    *
    * @return the resulting value
    */
  def semp(): IntValue = {
    capturedOut.clear()
    // Make an initial environment with a location for each function
    val boundEnv = program.funs.foldLeft(Map(): Env) { (a: Env, f: AFunDeclaration) =>
      a + (f -> newLoc())
    }
    // Create a location for each formal argument of main
    val envWithInputArgs = program.mainFunction.args.foldLeft(boundEnv) { (a: Env, id: AIdentifierDeclaration) =>
      a + (id -> newLoc())
    }
    // Store the functions in the associated locations
    val boundStore = program.funs.foldLeft(Map(): Store) { (s: Store, f: AFunDeclaration) =>
      s + (boundEnv(f) -> spec.mkFun(f))
    }
    // Store the input in the associated argument locations
    val storeWithInputArgs = program.mainFunction.args.foldLeft(boundStore) { (s: Store, id: AIdentifierDeclaration) =>
      s + (envWithInputArgs(id) -> input())
    }
    // Execute the main function
    val (_, cs) = semc(program.mainFunction.stmts, envWithInputArgs, storeWithInputArgs)
    // Return the result, if type int
    cs(returnLoc) match {
      case x: IntValue =>
        output(x)
        x
      case _ =>
        errorReturnNotInt(program.mainFunction)
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
    *
    * @param stm   the statement to execute
    * @param env   the initial environment
    * @param store the initial store
    * @return the resulting environment and store
    */
  protected def semc(stm: AStmt, env: Env, store: Store): (Env, Store) =
    stm match {
      case AAssignStmt(left, right: AExpr, _) =>
        val (lv, s1) = semeref(left, env, store)
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
            output(y)
            (env, s1)
          case y: ReferenceValue =>
            output(y)
            (env, s1)
          case _ => errorOutputNotInt()
        }
      case AVarStmt(ids, _) =>
        (ids.foldLeft(env) { (accenv, id) =>
          accenv + (id -> newLoc())
        }, store)
    }

  /**
    * Semantics for left-hand-side expressions.
    *
    * @param exp   the expression to execute
    * @param env   the environment
    * @param store the initial store
    * @return the resulting location and store
    */
  protected def semeref(exp: AExpr, env: Env, store: Store): (ReferenceValue, Store) =
    exp match {
      case id: AIdentifier => (env(id.declaration), store)
      case AUnaryOp(_: DerefOp.type, target, loc) =>
        semeright(target, env, store) match {
          case (pref: ReferenceValue, s1) => (pref, s1)
          case (_: NullValue, _) => errorNullDereference(loc)
          case (x, _) => errorDerefNotPointer(loc, target, x)
        }
      case _ => ???
    }

  /**
    * Semantics for right-hand-side expressions.
    * @param exp the expression to execute
    * @param env the environment
    * @param store the initial store
    * @return the resulting value and store
    */
  protected def semeright(exp: AExpr, env: Env, store: Store): (EValue, Store) =
    exp match {
      case access: AAccess =>
        val (tv, s1) = semeright(access.record, env, store)
        tv match {
          case rec: RecordValue =>
            if (rec.fields.contains(access.field)) {
              (rec.fields(access.field), s1)
            } else {
              errorAccessMissingField(access.loc, rec, access.field)
            }
          case _ =>
            errorAccessNonRecord(access.loc, tv)
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
        val cval = input()
        (cval, store)
      case AAlloc(content, _) =>
        val (cv, s1) = semeright(content, env, store)
        val l = newLoc()
        (l, s1 + (l -> cv))
      case ANull(_) => (spec.nullValue, store)
      case ANumber(value, _) => (spec.constInt(value), store)
      case AUnaryOp(_: RefOp.type, e: AExpr, _) =>
        semeref(e, env, store)
      case op @ AUnaryOp(_: DerefOp.type, _, _) =>
        val (l, s1) = semeref(op, env, store)
        (s1.getOrElse(l, errorDerefNotPointer(exp.loc, op, l)), s1)
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
        (s1.getOrElse(l, errorDerefNotPointer(exp.loc, id, l)), s1)
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
  private def input(): IntValue = {
    print(s"Enter input: ")
    val line = scala.io.StdIn.readLine()
    if (line == null) {
      spec.constInt(0)
    } else {
      Try(line.toInt) match {
        case Success(i) => spec.constInt(i)
        case Failure(_) => errorInputNotInt()
      }
    }
  }

  def errorCallNotFunction(funValue: EValue) =
    throw new RuntimeException(s"Call to a non-function $funValue")

  def errorOutputNotInt() =
    throw new RuntimeException(s"Output not supported for non-integer values")

  def errorInputNotInt() =
    throw new RuntimeException(s"Input not supported for non-integer values")

  def errorErrorNonInt(v: EValue) =
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

  def errorDerefNotPointer(loc: Loc, target: AExpr, x: EValue) =
    throw new RuntimeException(s"Dereferencing non-pointer $target at $loc: $x")

  def errorAccessNonRecord(loc: Loc, x: EValue) =
    throw new RuntimeException(s"Accessing field on a value that is not a record at $loc: $x")

  def errorAccessMissingField(loc: Loc, rec: RecordValue, field: String) =
    throw new RuntimeException(s"Missing field $field in record $rec at $loc")

  case class ExecutionError(code: IntValue) extends RuntimeException(s"Execution error, code: $code")

}

/**
  * Interpreter that uses concrete values.
  */
class ConcreteInterpreter(val program: AProgram)(implicit declData: DeclarationData) extends Interpreter(program) {
  val spec = ConcreteValues
}
