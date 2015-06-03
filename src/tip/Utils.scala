package tip

import java.io.{File, PrintWriter}

import tip.analysis.FlowSensitiveAnalysis
import tip.ast.{AFunDeclaration, AstNode}
import tip.graph._
import tip.logging.Log

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