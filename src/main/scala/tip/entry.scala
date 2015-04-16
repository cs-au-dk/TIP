package tip

import java.io.{ PrintWriter, FileFilter, File }
import org.parboiled2.{ErrorFormatter, ParseError}
import tip.analysis._
import tip.graph.IntraControlFlowGraph
import tip.ast.AIfStmt
import tip.parser.TipParser
import scala.io.Source
import scala.util.{ Failure, Success }
import tip.analysis._
import tip.graph.GNode
import tip.ast.AstNode
import tip.analysis.LivenessAnalysis
 
class Option {
  object DataFlowOption extends Enumeration {
    val Enabled, Disabled, EnabledWL = Value
    
    implicit def toBrief(x: DataFlowOption.Value):Boolean = {
      x match {
        case Disabled => false
        case _ => true
      }  
    }
  }
  var cfg = false
  var types = false
  var sign = DataFlowOption.Disabled
  var liveness = DataFlowOption.Disabled
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
               | [out] is an output directory (default: ./out)
               |
               | possible options are:
               | -cfg:           construct control flow graph, but do not perform any analysis
               | -types:         enable type analysis
               | -sign [wl]:     enable intraprocedural sign analysis using simple fixpoint solver,
               |                 the wl option enables the worklist-based fixpoint solver
               | -liveness [wl]: to output the cfg enriched with the liveness analysis information,
               |                 the wl option enables the worklist-based fixpoint solver
               |
             """.stripMargin)
  }
  
  def main(args: Array[String]): Unit = {

    val options = new Option()
    
    var i = 0
    def extDataFlowOptions(j: Int):options.DataFlowOption.Value = {
      if(j + 1 >= args.length) options.DataFlowOption.Enabled
      else {
        args(j+1) match {
          case "wl" =>
            i = i + 1
            options.DataFlowOption.EnabledWL
          case _ => options.DataFlowOption.Enabled
        }
      }
    }

    while (i < args.length) {
      args(i) match {
        case "-cfg" =>
          options.cfg = true
        case "-types" =>
          options.types = true
        case "-sign" => 
          options.sign = extDataFlowOptions(i)
        case "-liveness" =>
          options.liveness = extDataFlowOptions(i)
        case s: String =>
          if (i == args.length - 1 && options.source != null)
            options.out = new File(s)
          else if (i == args.length - 1 && options.source == null)
            options.source = new File(s)
          else if (i == args.length - 2)
            options.source = new File(s)
          else {
            println(s"Unrecognised option $s")
            printUsage()
            System.exit(1)
          }
      }
      i += 1
    }

    if (!options.check()) {
      printUsage()
      System.exit(1)
    }
    
    val sources = if (options.source.isDirectory) {
      options.source.listFiles(new FileFilter {
        def accept(fl: File): Boolean = fl.getName.endsWith(".tip")
      })
    } else {
      Array(options.source)
    }

    sources.foreach { file =>
      println(s"Processing ${file.getName}")

      val program = Source.fromFile(file).mkString
      val tipParser = new TipParser(program)
      val res = tipParser.InputLine.run()

      res match {
        case Failure(e: ParseError) =>
          println(s"Failure parsing the program: $file\n$program")
          println(tipParser.formatError(e, new ErrorFormatter(showTraces = true)));
        case Failure(e: Throwable) =>
          println(s"Failure parsing the program: $file\n$program")
          e.printStackTrace()
        case Success(programNode) =>
          try {
            DeclarationAnalysis(programNode)
            val ta = TypeAnalysis(programNode)
            
            if (options.cfg | options.sign | options.liveness) {
              val cfgs = IntraControlFlowGraph.generateFromProgram(programNode, IntraControlFlowGraph.astLabeler)

              // Once given a solution this is how we wish to label the nodes
              def labeler(res: Map[GNode[AstNode], _])(entry: GNode[AstNode]) = {s"$entry\n${res(entry)}"}
              
              if (options.cfg) {
                cfgs.foreach { case (fun, cfg) =>
                  Utils.output(file, fun.name.toString(), OutputKind.Cfg, cfg.toDot(), options.out)
                }
              }
              if (options.sign) {
                cfgs.foreach { case(fun, cfg) =>
                  val res = options.sign match {
                    case options.DataFlowOption.Enabled => new IntraprocSignAnalysisSimpleSolver(cfg).analyze()
                    case options.DataFlowOption.EnabledWL => new IntraprocSignAnalysisWorklistSolver(cfg).analyze()
                    case _ => ???
                  }
                  Utils.output(file, fun.name.toString(), OutputKind.Sign, cfg.toDot(labeler(res)), options.out)
                }
              }
              if(options.liveness) {
                cfgs.foreach { case(fun, cfg) =>
                  val res = options.liveness match {
                    case options.DataFlowOption.Enabled => ???//new LivenessAnalysisSimpleSolver(cfg).analyze()
                    case options.DataFlowOption.EnabledWL => ???//new LivenessAnalysisWorklistSolver(cfg).analyze()
                    case _ => ???
                  } 
                  Utils.output(file, fun.name.toString(), OutputKind.Liveness, cfg.toDot(labeler(res)), options.out)
                }
              }
            }
            if (options.types) {
              val ttip = programNode.toTypedString()
              Utils.output(file, "", OutputKind.Ast, ttip, options.out)
              val cnst = ta.generatedConstraints()
              val cnstOut = cnst.mkString("\n")
              Utils.output(file, "", OutputKind.Constraints, cnstOut, options.out)
            }
            
            println("Success")       
          
          } catch {
            case e: Exception => 
              println(s"Error processing $file\n")
              e.printStackTrace()
          }
      }
    }
  }
}
