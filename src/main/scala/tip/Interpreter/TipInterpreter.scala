package tip.Interpreter

import tip.newAST._

import scala.collection.mutable

/*
  Created by Erik on 15-04-2015.

  An Interpreter for the TIP language.
  Notes:
    This interpreter throws an exception if you do stuff with an undefined value. This includes returning it from a function.
      You can however do stuff with pointers to undefined values.
    The error handling consists of throwing RuntimeExceptions.
    There is currently no restrictions on what the main method can return (except for undefined values).
 */
class TipInterpreter(program: AProgram) {

  private val functions = mutable.Map[String, AFunDeclaration]()

  def run() : Integer = {
    program.fun.foreach((function: AFunDeclaration) => {
      functions += (function.name.toString() -> function)
    })
    println("Running program")
    val returnValue: Value = runFunction(program.fun.last, Seq())
    println(s"Program ran succesfully, result: $returnValue")
    returnValue match {
      case IntValue(i) => i
      case _ => -1
    }
  }

  private def runFunction(function : AFunDeclaration, arguments : Seq[Value]) : Value = {
    val environment: mutable.Map[String, Value] = mutable.Map[String, Value]()
    assert(function.args.length == arguments.length)
    function.args.zip(arguments).foreach {
      case (arg: AIdentifier, argValue : Value) =>
        environment += (arg.value -> argValue)
    }

    function.stmts.content.dropRight(1).foreach((stm : AStmt) => {
      runStatement(stm, environment)
    })
    assert(function.stmts.content.last.isInstanceOf[AReturnStmt])
    val returnStmt: AReturnStmt = function.stmts.content.last.asInstanceOf[AReturnStmt]
    runExpression(returnStmt.value, environment)
  }

  private def runStatement(stm: AStmt, environment: mutable.Map[String, Value]) : Unit = {
    stm match {
      case AAssignStmt(left: AExpr, right: AExpr, _) =>
        val value: Value = runExpression(right, environment)
        left match {
          case AIdentifier(id, _) => environment += (id -> value)
          case AUnaryOp(DerefOp(), AIdentifier(id, _), loc) => environment(id).asInstanceOf[PointerValue].of match {
            case NullValue() => sys.error(s"NullPointer exception $loc")
            case v : IntValue => v.value = value.asInstanceOf[IntValue].value;
            case v : PointerValue => v.of = value
            case v : MemoryCell => v.value = Some(value);
            case FunctionValue(_) => sys.error("Cannot do this!")
          }
          case _ => sys.error("An l-value can only an id or an id reference")
        }
      case ABlockStmt(content, _) => content.foreach((stm) => runStatement(stm, environment))
      case AIfStmt(guard, ifBranch, elseBranch, _) =>
        val value: Integer = runExpression(guard, environment).asInstanceOf[IntValue].value
        if (value != 0) {
          runStatement(ifBranch, environment)
        } else {
          elseBranch match {
            case Some(stm: AStmt) => runStatement(stm, environment);
            case None =>
          }
        }
      case AReturnStmt(_, _) => sys.error("Return statement can only be as the last statement of a function")
      case AVarStmt(ids, _) => ids.foreach((id: AIdentifier) => environment += (id.value -> MemoryCell()))
      case AWhileStmt(guard, inner, _) =>
        while(runExpression(guard, environment).asInstanceOf[IntValue].value != 0) {
          runStatement(inner, environment)
        }
      case AoutputStmt(value, _) => println(runExpression(value, environment).asInstanceOf[IntValue].value);
    }
  }

  private def runExpression(exp: AExpr, environment: mutable.Map[String, Value]): Value = {
    val res: Value = exp match {
      case e: ABinaryOp =>
        val left: Value = runExpression(e.left, environment)
        val right: Value = runExpression(e.right, environment)
        e.operator match {
          case Eqq() => IntValue(if (left == right) 1 else 0)
          case Divide() => IntValue(left.asInstanceOf[IntValue].value / right.asInstanceOf[IntValue].value)
          case GreatThan() => IntValue(if (left.asInstanceOf[IntValue].value > right.asInstanceOf[IntValue].value) 1 else 0)
          case Minus() => IntValue(left.asInstanceOf[IntValue].value - right.asInstanceOf[IntValue].value)
          case Plus() => IntValue(left.asInstanceOf[IntValue].value + right.asInstanceOf[IntValue].value)
          case Times() => IntValue(left.asInstanceOf[IntValue].value * right.asInstanceOf[IntValue].value)
          case DerefOp() | RefOp() => sys.error("Not possible... ")
        }
      case AIdentifier(id, loc) =>
        if (environment.contains(id)) {
          environment(id)
        } else if (functions.contains(id)) {
          FunctionValue(functions(id))
        } else {
          sys.error(s"Unresolved identifier $id at $loc")
        }
      case AInput(_) => val line = scala.io.StdIn.readLine()
        if (line == null) IntValue(0) else IntValue(line.toInt)
      case AMalloc(_) => PointerValue(MemoryCell())
      case ANull(_) => NullValue()
      case ANumber(value, _) => IntValue(value)
      case AUnaryOp(op, target: AExpr, _) =>
        op match {
          case RefOp() => PointerValue(runExpression(target, environment))
          case DerefOp() => runExpression(target, environment).asInstanceOf[PointerValue].of
          case _ => sys.error("This is not possible...")
        }
      case ACallFuncExpr(target, args, _) =>
        val func: AFunDeclaration = runExpression(target, environment).asInstanceOf[FunctionValue].func
        runFunction(func, args.map((arg) => runExpression(arg, environment)))
    }
    res.realValue()
  }

  
  sealed trait Value {
    def realValue() : Value = {
      this match {
        case cell: MemoryCell =>
          cell.value match {
            case Some(value) => value.realValue()
            case None => sys.error("Attempted to access an undefined value!")
          }
        case _ =>
          this
      }
    }
  }
  case class IntValue(var value: Integer) extends Value
  case class FunctionValue(var func: AFunDeclaration) extends Value
  case class PointerValue(var of: Value) extends Value
  case class NullValue() extends Value
  case class MemoryCell(var value : Option[Value] = None) extends Value
}
