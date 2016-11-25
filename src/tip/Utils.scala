package tip

import java.io.{File, PrintWriter}

import tip.analysis.FlowSensitiveAnalysis
import tip.ast.{AFunDeclaration, AstNode}
import tip.graph._
import tip.logging.Log
import tip.ast.AProgram
import tip.parser.TipParser
import scala.util.Success
import tip.analysis.DeclarationAnalysis
import scala.io.Source

object OutputKindE extends Enumeration {
  val Cfg, Icfg, Ast, Constraints = Value
}

sealed trait OutputKind

case class OtherOutput(kind: OutputKindE.Value) extends OutputKind {
  override def toString: String = kind.toString
}

case class DataFlowOutput(kind: FlowSensitiveAnalysis.Analysis.Value) extends OutputKind {
  override def toString: String = kind.toString
}

object Utils {

  val log = Log.typeLogger[this.type](Log.Level.Info)

  /**
   * Generate an output for a file into a new file with the given id in the outFolder.
   * The content is written into the output, assuming the function is writing a specific kind of output
   */
  def output(file: File, id: String, kind: OutputKind, content: String, outFolder: File): Unit = {

    val extension = kind match {
      case OtherOutput(OutputKindE.Cfg) => "_cfg.dot"
      case OtherOutput(OutputKindE.Icfg) => "_icfg.dot"
      case OtherOutput(OutputKindE.Ast) => "_type.ttip"
      case OtherOutput(OutputKindE.Constraints) => "_constraints.md"
      case DataFlowOutput(k) =>
        s"_$k.dot"
      case _ => ???
    }
    val outFile = new File(outFolder, s"${file.getName}_$id$extension")
    val pw = new PrintWriter(outFile, "UTF-8")
    pw.write(content)
    pw.close()

    log.info(s"Results of $kind for $file $id written into $outFile")
  }

  /**
   * Escapes special characters in the given string.
   * Special characters are all Unicode chars except 0x20-0x7e but including \, ", {, and }.
   */
  def escape(s: String): String = {
    if (s == null)
      return null
    val b = new StringBuilder()
    var i = 0
    for (i <- 0 to s.length - 1) {
      val c = s.charAt(i)
      c match {
        case '"' =>
          b.append("\\\"")
        case '\\' =>
          b.append("\\\\")
        case '\b' =>
          b.append("\\b")
        case '\t' =>
          b.append("\\t")
        case '\n' =>
          b.append("\\n")
        case '\r' =>
          b.append("\\r")
        case '\f' =>
          b.append("\\f")
        case '<' =>
          b.append("\\<")
        case '>' =>
          b.append("\\>")
        case '{' =>
          b.append("\\{")
        case '}' =>
          b.append("\\}")
        case _ =>
          if (c >= 0x20 && c <= 0x7e)
            b.append(c)
          else {
            b.append("\\u")
            val t = Integer.toHexString(c & 0xffff)
            var j = 0
            while (j + t.length() < 4) {
              b.append('0')
              b.append(t)
              j += 1
            }
          }
      }
    }
    b.toString()
  }

  // Once given a solution this is how we wish to label the nodes
  def labeler(res: Map[GNode[AstNode], _])(entry: GNode[AstNode]) = {
    entry match {
      case entry: FunEntry[AstNode] => s"Function ${entry.data.asInstanceOf[AFunDeclaration].name} entry\n${res(entry)}"
      case exit: FunExit[AstNode] => s"Function ${exit.data.asInstanceOf[AFunDeclaration].name} exit\n${res(entry)}"
      case _ => s"$entry\n${res(entry)}"
    }
  }

  def dotIder(entry: GNode[AstNode]): String = {
    entry match {
      case real: GRealNode[AstNode] => s"real${real.data.offset.col}_${real.data.offset.line}"
      case entry: FunEntry[AstNode] => s"entry${entry.data.offset.col}_${entry.data.offset.line}"
      case exit: FunExit[AstNode] => s"exit${exit.data.offset.col}_${exit.data.offset.line}"
      case call: CallNode[AstNode] => s"cally${call.data.offset.col}_${call.data.offset.line}"
      case acall: AfterCallNode[AstNode] => s"acall${acall.data.offset.col}_${acall.data.offset.line}"
      case _ => System.currentTimeMillis().toString
    }
  }
}

object MapUtils {

  implicit class ReverseOp[A, B](m: Map[A, Set[B]]) {
    def reverse: Map[B, Set[A]] = {
      var res = Map[B, Set[A]]()
      m.keys.foreach { k =>
        m(k).foreach { v =>
          val ins = res.getOrElse(v, Set[A]())
          res += (v -> (ins + k))
        }
      }
      res
    }
  }

  implicit class ReverseOp2[A, B](m: Map[A, B]) {
    def reverse: Map[B, Set[A]] = {
      var res = Map[B, Set[A]]()
      m.keys.foreach { k =>
        val ins = res.getOrElse(m(k), Set[A]())
        res += (m(k) -> (ins + k))
      }
      res
    }
  }
}

object InterpreterUtils {
  /**
   * Parses the TIP source file at the given location, performs a declaration analysis, 
   * and returns an Abstract Syntax Tree in which every AIdentifier 
   * has been annotated with its corresponding declaration. 
   * Required for the Interpreter.
   */
  def prepare(fileName : String) : AProgram =  {
    val src = Source.fromFile(fileName).mkString
    val tipParser = new TipParser(src)
    val p = tipParser.InputLine.run()
    p match {
      case Success(programNode : tip.ast.AProgram) => {
        new DeclarationAnalysis(programNode)
        programNode
      }
    }
  }
}

object SMTUtils {
  import smtlib.Interpreter
  import smtlib.parser.Parser
  import smtlib.parser.Commands._
  import smtlib.parser.CommandsResponses._
  import smtlib.parser.Terms._
  import smtlib.theories.Core._

  val log = Log.typeLogger[this.type](Log.Level.Info)

  private def runScriptGetModel(script: Script)(implicit interpreter: Interpreter): Option[List[SExpr]] = {
    script.commands.foreach(interpreter.eval _)
    interpreter.eval(CheckSat()) match {
      case CheckSatStatus(SatStatus) => interpreter.eval(GetModel()) match {
        case GetModelResponseSuccess(m) => Some(m)
        case s => log.info(s"Unhandled sat response: $s"); None
      }
      case CheckSatStatus(UnsatStatus) => None
      case s => log.info(s"Unhandled response code: $s"); None
    }
  }


  private def extractConstModel(model: List[SExpr]): Map[String, BigInt] = {
    def extract(cl: SExpr): Option[(String, BigInt)] = cl match {
      case DefineFun(fundef) =>
        if (fundef.params.isEmpty) {
          (fundef.body, fundef.returnSort) match {
            case (SNumeral(v), smtlib.theories.Ints.IntSort()) =>
              /* Positive integer */
              Some((fundef.name.name, v))
            case (FunctionApplication(QualifiedIdentifier(SimpleIdentifier(SSymbol("-")), _),
              List(SNumeral(v))), smtlib.theories.Ints.IntSort()) =>
              /* Negative numbers are represented as (- x) */
              Some((fundef.name.name, - v))
            case (SHexadecimal(v), smtlib.theories.FixedSizeBitVectors.BitVectorSort(n)) =>
              /* Bitvector number */
              Some((fundef.name.name, v.toInt))
              /* There's probably more missing cases */
            case _ => log.info(s"Ignoring non-integer model values ($fundef)"); None
          }
        } else {
          log.info(s"Ignoring non-constant values in the model ($fundef)"); None
        }
      case _ => log.info(s"Ignoring unknown model clause ($cl)"); None
    }
    model match {
      case Nil => Map[String, BigInt]()
      case first :: rest => extract(first) match {
        case Some(m) => extractConstModel(rest) + m
        case None => extractConstModel(rest)
      }
    }
  }

  implicit lazy val z3 = smtlib.interpreters.Z3Interpreter.buildDefault
  /** Solves a SMTLib script */
  def solve(script: Script): Option[Map[String, BigInt]] = {
    log.info(s"solving $script")
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
   * Reset the status of the solver. If not called, previous constraints still
   * hold.
   */
  def reset(): Unit = z3.eval(Reset())
}
