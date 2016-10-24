package tip.interpreter

import tip.ast._
import tip.logging.Log

/*
  An Interpreter for the TIP language.
  Notes:
    This interpreter throws an exception if you do stuff with an undefined value. This includes returning it from a function.
    You can however do stuff with pointers to undefined values.
    The error handling consists of throwing RuntimeExceptions.
    There is currently no restrictions on what the main method can return (except for undefined values).
 */
class Interpreter(program: AProgram) {

  val log = Log.typeLogger[this.type](Log.Level.Info)

  trait Value
  case class IntValue(i: Int) extends Value
  case class FunValue(fun: AFunDeclaration) extends Value
  case class Location(var i: Option[Value]) extends Value {
    override def equals(obj: scala.Any): Boolean = obj match {
      case rf: AnyRef => this.eq(rf)
      case _ => false
    }
  }
  case object NullValue extends Value

  type Env = Map[AIdentifierDeclaration, Location]

  val returnId = AIdentifier("return", Loc(-1, -1))()

  def run() : Integer = {
    log.info("Running program")
    val value = runFunction(program.mainFunction, Seq())
    value match {
      case IntValue(i) =>
        log.info(s"Program ran succesfully, result: $i"); i
      case _ => throw new RuntimeException("Main function can only return an integer")
    }
  }

  private def runFunction(fun: AFunDeclaration, args: Seq[Value]): Value = {
    val boundEnv = program.fun.foldLeft(Map[AIdentifierDeclaration, Location]()) { (a: Env, f: AFunDeclaration) =>
      a + (f -> new Location(Some(FunValue(f))))
    } ++
      fun.args.zip(args).foldLeft(Map[AIdentifierDeclaration, Location]()) { (a: Env, p: (AIdentifier, Value)) =>
        a + (p._1 -> new Location(Some(p._2)))
      } +
      (returnId -> new Location(None))


    runStatement(fun.stmts, boundEnv)(returnId).i match {
      case Some(x) => x
      case None => missingReturn(fun)
    }
  }

  private def runStatement(stm: AStmt, env: Env) : Env = {
    stm match {
      case AAssignStmt(left: AAssignable, right: AExpr, _) =>
        val value: Value = runExpression(right, env)
        left match {
          case id: AIdentifier =>
            env(id.meta.definition.get).i = Some(value)
          case AUnaryOp(DerefOp(), id: AIdentifier, loc) =>
            env(id.meta.definition.get).i match {
              case Some(location @ Location(x)) => location.i = Some(value)
              case Some(NullValue) => nullPointerException(loc)
              case _ => unreferenceNonPointer(loc)
            }
          case _ => throw new RuntimeException(s"Unassignable on the left-hand side of an assignmnet: $left")
        }
        env
      case ABlockStmt(content, _) => content.foldLeft(env)((env: Env, stm: AStmt) => runStatement(stm, env))
      case AIfStmt(guard, ifBranch, elseBranch, loc) =>
        val value: Value = runExpression(guard, env)
        value match {
          case IntValue(0) =>
            elseBranch.map(stmt => runStatement(stmt, env)).getOrElse(env)
          case IntValue(1) =>
            runStatement(ifBranch, env)
          case _ => guardNotInteger(loc)
        }
      case ret: AReturnStmt =>
        env(returnId).i = Some(runExpression(ret.value, env))
        env
      case AVarStmt(ids, _) =>
        ids.foldLeft(env)((env: Env, id: AIdentifier) => env + (id.meta.definition.get -> new Location(None)))
      case w: AWhileStmt =>
        val gvalue = runExpression(w.guard, env)
        gvalue match {
          case IntValue(0) => env
          case IntValue(_) => runStatement(w, runStatement(w.innerBlock, env))
          case _ => guardNotInteger(w.offset)
        }
      case AoutputStmt(value, _) =>
        val out = runExpression(value, env)
        out match {
          case IntValue(x) => log.info(s"Program out: $x"); env
          case _ => throw new RuntimeException(s"Output not supported for non integer values")
        }
    }
  }

  private def runExpression(exp: AExpr, env: Env): Value = {
    exp match {
      case e: ABinaryOp =>
        val left: Value = runExpression(e.left, env)
        val right: Value = runExpression(e.right, env)
        e.operator match {
          case Eqq() => IntValue(if (left == right) 1 else 0)
          case op: Operator => {
            (left, right) match {
              case (IntValue(lv), IntValue(rv)) => {
                op match {
                  case Divide() => IntValue(lv / rv)
                  case GreatThan() => IntValue(if (lv > rv) 1 else 0)
                  case Minus() => IntValue(lv - rv)
                  case Plus() => IntValue(lv + rv)
                  case Times() => IntValue(lv * rv)
                  case _ => ???
                }
              }
              case _ => throw new RuntimeException(s"Unable to apply the operator $op to non integer values")
            }
          }
        }
      case id: AIdentifier =>
        env(id.meta.definition.get).i match {
          case Some(z) => z
          case None => throw new RuntimeException(s"Not initialised variable at ${id.offset}")
        }
      case AInput(_) => val line = scala.io.StdIn.readLine()
        if (line == null) IntValue(0) else IntValue(line.toInt)
      case AMalloc(_) => new Location(None)
      case ANull(_) => NullValue
      case ANumber(value, _) => IntValue(value)
      case AUnaryOp(op: DerefOp, target: AExpr, loc) =>
        runExpression(target, env) match {
          case Location(Some(x)) => x
          case NullValue => nullPointerException(loc)
          case _ => unreferenceNonPointer(loc)
        }
      case AUnaryOp(op: RefOp, target: AExpr, loc) =>
        target match {
          case id: AIdentifier => env(id.meta.definition.get)
          case _ => throw new RuntimeException(s"Can not take the reference of an expression at $loc")
        }
      case ACallFuncExpr(target, args, loc) =>
        val funValue = runExpression(target, env)
        funValue match {
          case f: FunValue =>
            runFunction(f.fun, args.map((arg) => runExpression(arg, env)))
          case _ => throw new RuntimeException(s"Call to a non-function at $loc, $funValue found")
        }
    }
  }

  def missingReturn(fun: AFunDeclaration) = throw new RuntimeException(s"Missing return statement in ${fun.name}")
  def nullPointerException(loc: Loc) = throw new RuntimeException(s"NullPointer exception at $loc")
  def guardNotInteger(loc: Loc) = throw new RuntimeException(s"Guard in $loc not evaluating to an integer")
  def unreferenceNonPointer(loc: Loc) = throw new RuntimeException(s"Unreferencing a non-pointer at $loc")
}
