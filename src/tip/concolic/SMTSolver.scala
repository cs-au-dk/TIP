package tip.concolic

import tip.ast._
import smtlib.Interpreter
import smtlib.interpreters.Z3Interpreter
import smtlib.parser.Parser
import smtlib.parser.Commands._
import smtlib.parser.CommandsResponses._
import smtlib.parser.Terms._
import tip.util.Log

object SMTSolver {

  val log = Log.logger[this.type]()

  /**
    * Expressions extended with symbols.
    */
  class Symbol(location: Loc, counter: Int) extends AIdentifier(s"s$counter", location) {
    override def toString: String = s"s$counter"
  }

  def pathToSMT(vars: List[Symbol], path: List[(AExpr, Boolean)]): String = {
    def opToSexp(op: BinaryOperator): String =
      op match {
        case Eqq => "="
        case _ => op.toString
      }

    def expToSexp(exp: AExpr): String =
      exp match {
        case ABinaryOp(op, left, right, _) =>
          s"(${opToSexp(op)} ${expToSexp(left)} ${expToSexp(right)})"
        case n: ANumber => n.value.toString
        case n: Symbol => n.name.toString
        case _ => exp.toString
      }

    def symbolsToSMT(vars: List[Symbol]): String =
      vars.map(sv => s"(declare-const ${sv.name} Int)").mkString("\n")

    path.foldLeft(symbolsToSMT(vars))((script: String, cond: (AExpr, Boolean)) => {
      val branchrecord =
        if (cond._2)
          expToSexp(cond._1)
        else
          "(not " + expToSexp(cond._1) + ")"
      script + "\n" + "(assert " + branchrecord + ")"
    })
  }

  def runScriptGetModel(script: Script)(implicit interpreter: Interpreter): Option[List[SExpr]] = {
    script.commands.foreach(interpreter.eval)
    interpreter.eval(CheckSat()) match {
      case CheckSatStatus(SatStatus) =>
        interpreter.eval(GetModel()) match {
          case GetModelResponseSuccess(m) => Some(m)
          case s => log.info(s"Unhandled sat response: $s"); None
        }
      case CheckSatStatus(UnsatStatus) => None
      case s => log.info(s"Unhandled response code: $s"); None
    }
  }

  def extractConstModel(model: List[SExpr]): Map[String, BigInt] = {
    def extract(cl: SExpr): Option[(String, BigInt)] = cl match {
      case DefineFun(fundef) =>
        if (fundef.params.isEmpty) {
          (fundef.body, fundef.returnSort) match {
            case (SNumeral(v), smtlib.theories.Ints.IntSort()) =>
              /* Positive integer */
              Some((fundef.name.name, v))
            case (FunctionApplication(QualifiedIdentifier(SimpleIdentifier(SSymbol("-")), _), List(SNumeral(v))), smtlib.theories.Ints.IntSort()) =>
              /* Negative numbers are represented as (- x) */
              Some((fundef.name.name, -v))
            case (SHexadecimal(v), smtlib.theories.FixedSizeBitVectors.BitVectorSort(_)) =>
              /* Bitvector number */
              Some((fundef.name.name, v.toInt))
            /* There's probably more missing cases */
            case _ => log.info(s"Ignoring non-integer model values ($fundef)"); None
          }
        } else {
          log.info(s"Ignoring non-constant values in the model ($fundef)")
          None
        }
      case _ => log.info(s"Ignoring unknown model clause ($cl)"); None
    }

    model match {
      case Nil => Map[String, BigInt]()
      case first :: rest =>
        extract(first) match {
          case Some(m) => extractConstModel(rest) + m
          case None => extractConstModel(rest)
        }
    }
  }

  implicit lazy val z3: Z3Interpreter = smtlib.interpreters.Z3Interpreter.buildDefault

  /** Solves a SMTLib script */
  def solve(script: Script): Option[Map[String, BigInt]] = {
    reset()
    runScriptGetModel(script).map(extractConstModel)
  }

  /**
    * Solves a SMTLib script represented as a string. For example:
    * val s = "(declare-const x Int) (assert (> x 0))"
    * tip.SMTUtils.solve(s) // Returns Some(Map(x -> 1))
    */
  def solve(s: String): Option[Map[String, BigInt]] =
    solve(Parser.fromString(s).parseScript)

  /**
    * Reset the status of the solver. If not called, previous constraints still hold.
    */
  def reset(): Unit = z3.eval(Reset())
}
