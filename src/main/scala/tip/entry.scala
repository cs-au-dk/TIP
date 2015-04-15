package tip

import java.io.{ PrintWriter, FileFilter, File }

import org.parboiled2.ParseError
import tip.Interpreter.TipInterpreter
import tip.analysis.{ TypeAnalysis, DeclarationAnalysis }
import tip.graph.IntraControlFlowGraph
import tip.newAST.AIfStmt
import tip.parser.TipParser

import scala.io.Source
import scala.util.{ Failure, Success }

class Option {
  var cfg = false;
  var types = false;
  var run = false;
  var source: File = null
  var out: File = Globals.defaultOut

  def check(): Boolean = {
    source != null
  }
}

object entry {

  def printUsage() = {
    println("""
               | Usage:
               | tip <options> <source> [out]
               |
               | <source> can be a file or a directory,
               |
               | [out] is an optional custom output directory,
               | by default ./out is used
               |
               | possible options are:
               | -cfg : to output the control flow graph
               | -types : to output the cfg with the types at the declaration nodes
               | -run : run the program as the last step
               |
             """.stripMargin)

  }

  def main(args: Array[String]): Unit = {

    val options = new Option()

    var i = 0
    while (i < args.length) {
      args(i) match {
        case "-cfg" =>
          options.cfg = true
        case "-types" =>
          options.types = true
        case s: String =>
          if (i == args.length - 1 && options.source != null)
            options.out = new File(s)
          else if (i == args.length - 1 && options.source == null)
            options.source = new File(s)
          else if (i == args.length - 2)
            options.source = new File(s)
          else
            println(s"Unrecognised option $s")
      }
      i += 1
    }

    if (!options.check())
      printUsage()
    else {

      val sources = if (options.source.isDirectory) {
        options.source.listFiles(new FileFilter {
          def accept(fl: File): Boolean = fl.getName.endsWith(".tip")
        })
      } else {
        Array(options.source)
      }

      sources.foreach { file =>
        println(s"\nProcessing ${file.getName}")

        val program = Source.fromFile(file).mkString
        val tipParser = new TipParser(program)
        val res = tipParser.InputLine.run()

        res match {
          case Failure(e: ParseError) =>
            println(s"Failure parsing the program: $file\n$program")
            println(tipParser.formatError(e, showTraces = true));
          case Failure(e: Throwable) =>
            println(s"Failure parsing the program: $file\n$program")
          case Success(programNode) =>
            try {
              DeclarationAnalysis(programNode)
              val ta = TypeAnalysis(programNode)

              if (options.cfg) {
                val cfgs = IntraControlFlowGraph.generateFromProgram(programNode, IntraControlFlowGraph.prettyLabeller)

                cfgs.foreach { cfg =>
                  Utils.output(file, cfg._1.name.toString(), OutputKind.Cfg, cfg._2.toDot(), options.out)
                }
              }
              if (options.types) {
                val ttip = programNode.toTypedString()
                Utils.output(file, "", OutputKind.Ast, ttip, options.out)
                val cnst = ta.generatedConstraints()
                val cnstOut = cnst.mkString("\n")
                Utils.output(file, "", OutputKind.Constraints, cnstOut, options.out)
              }
              if (options.run) {
                new TipInterpreter(programNode).run()
              }
            } catch {
              case e: Exception => println(s"Error processing $file\n$e")
            }
        }
      }

      println("Success")
    }
  }
}
