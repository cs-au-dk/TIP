package tip.util

import java.io.{File, PrintWriter}

import tip.analysis.FlowSensitiveAnalysis
import tip.cfg._

/**
  * Basic outputting functionality.
  */
object Output {

  val log = Log.logger[this.type](Log.Level.Info)

  /**
    * Generate an output to a file.
    * @param file the output file
    * @param kind output kind (determines the file name suffix)
    * @param outFolder the output directory
    */
  def output(file: File, kind: OutputKind, content: String, outFolder: File): Unit = {
    val extension = kind match {
      case OtherOutput(OutputKindE.Cfg) => "_cfg.dot"
      case OtherOutput(OutputKindE.Icfg) => "_icfg.dot"
      case OtherOutput(OutputKindE.Ast) => "_type.ttip"
      case DataFlowOutput(k) =>
        s"_$k.dot"
      case _ => ???
    }
    val outFile = new File(outFolder, s"${file.getName}_$extension")
    val pw = new PrintWriter(outFile, "UTF-8")
    pw.write(content)
    pw.close()
    log.info(s"Results of $kind for $file written into $outFile")
  }

  /**
    * Escapes special characters in the given string.
    * Special characters are all Unicode chars except 0x20-0x7e but including \, ", {, and }.
    */
  def escape(s: String): String = {
    if (s == null)
      return null
    val b = new StringBuilder()
    for (i <- 0 until s.length) {
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
            b.append("\\%04X".format(c.toInt))
          }
      }
    }
    b.toString()
  }

  /**
    * Helper function for producing string output for a control-flow graph node after an analysis.
    * @param res map from control-flow graph nodes to strings, as produced by the analysis
    */
  def labeler(res: Map[CfgNode, _])(n: CfgNode) = {
    n match {
      case entry: CfgFunEntryNode => s"Function ${entry.data.name} entry\n${res(n)}"
      case exit: CfgFunExitNode => s"Function ${exit.data.name} exit\n${res(n)}"
      case _ => s"$n\n${res(n)}"
    }
  }

  /**
    * Generate an unique ID string for the given AST node.
    */
  def dotIder(n: CfgNode): String = {
    n match {
      case real: CfgStmtNode => s"real${real.data.loc.col}_${real.data.loc.line}"
      case entry: CfgFunEntryNode => s"entry${entry.data.loc.col}_${entry.data.loc.line}"
      case exit: CfgFunExitNode => s"exit${exit.data.loc.col}_${exit.data.loc.line}"
      case call: CfgCallNode => s"cally${call.data.loc.col}_${call.data.loc.line}"
      case acall: CfgAfterCallNode => s"acall${acall.data.loc.col}_${acall.data.loc.line}"
    }
  }
}

/**
  * Different kinds of output (determine output file names).
  */
object OutputKindE extends Enumeration {
  val Cfg, Icfg, Ast = Value
}

sealed trait OutputKind

/**
  * Output kind for a dataflow analysis (named according to the analysis).
  */
case class DataFlowOutput(kind: FlowSensitiveAnalysis.Analysis.Value) extends OutputKind {
  override def toString: String = kind.toString
}

/**
  * Other output kinds (for other processing phases than the actual analysis).
  */
case class OtherOutput(kind: OutputKindE.Value) extends OutputKind {
  override def toString: String = kind.toString
}
