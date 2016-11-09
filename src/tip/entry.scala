package tip

import java.io.{File, FileFilter}

import org.parboiled2.{ErrorFormatter, ParseError}
import tip.analysis.FlowSensitiveAnalysis.{Analysis => dfa, AnalysisOption => dfo}
import tip.analysis._
import tip.graph._
import tip.interpreter.Interpreter
import tip.logging.Log
import tip.parser.TipParser

import scala.io.Source
import scala.language.existentials
import scala.util.{Failure, Success}
import tip.ast.AProgram
import tip.concolic.SymbolicInterpreter

class RunOption {

var cfg = false
var icfg = false
var types = false
var cfa = false
var andersen = false
var steensgaard = false
var dfAnalysis = Map[dfa.Value, dfo.Value] ().withDefaultValue (dfo.Disabled)
var source: File = null
var out: File = Globals.defaultOut

var run = false
var concolic = false

def check (): Boolean = {
source != null
}
}

object entry {

  val log = Log.typeLogger[this.type](Log.Level.Info)

  def printUsage() = {
    println(
      """
        | Usage:
        | tip <options> <source> [out]
        |
        | <source> can be a file or a directory,
        |
        | [out] is an output directory (default: ./out)
        |
        | possible options are:
        |
        | Analyses:
        |
        | -cfg               construct the control flow graph, but do not perform any analysis
        | -icfg              construct the interprocedural control flow graph, but do not perform any analysis
        | -types             enable type analysis
        | -cfa               enable control-flow analysis
        | -andersen          enable Andersen pointer analysis
        | -steensgaard       enable Steensgaard pointer analysis
        | -sign              enable sign analysis
        | -livevars          enable live variables analysis
        | -available         enable available expressions analysis
        | -vbusy             enable very busy expressions analysis
        | -reaching          enable reaching definitions analysis
        | -constprop         enable constant propagation analysis
        | -interval          enable interval analysis
        |
        | some of the previous options can be followed immediately by the modifiers
        |
        | wl       use the worklist solver
        | wli      use the worklist solver with init
        | wliw     use the worklist solver with init and widening
        | wliwn    use the worklist solver with init, widening, and narrowing
        | wlip     use the worklist solver with init and propagation
        | iwli     use the worklist solver with init, interprocedural version
        | iwlip    use the worklist solver with init and propagation, interprocedural version
        | iwlic    use the worklist solver with init, interprocedural version with CFA analysis
        |
        | e.g. -sign wl  will run the sign analysis using the basic worklist solver
        |
        | Running:
        |
        | -run               run the program as the last step
        | -concolic          perform concolic testing (i.e., search for failing inputs using dynamic symbolic execution)
        |
      """.
        stripMargin)
  }

  def main(args: Array[String]): Unit = {

    val options = new RunOption()

    var i = 0
    def extDataFlowOptions(j: Int): dfo.Value = {
      if (j + 1 >= args.length) dfo.simple
      else if (dfo.values.map(_.toString()).contains(args(j + 1))) {
        i = i + 1
        dfo.withName(args(j + 1))
      }
      else
        dfo.simple
    }

    while (i < args.length) {
      args(i) match {
        case "-cfg" =>
          options.cfg = true
        case "-icfg" =>
          options.icfg = true
        case "-types" =>
          options.types = true
        case "-cfa" =>
          options.cfa = true
        case "-andersen" =>
          options.andersen = true
        case "-steensgaard" =>
          options.steensgaard = true
        case "-sign" | "-livevars" | "-available" | "-vbusy" | "-reaching" | "-constprop" | "-interval" =>
          options.dfAnalysis += dfa.withName(args(i).drop(1)) -> extDataFlowOptions(i)
        case "-run" =>
          options.run = true
        case "-concolic" =>
          options.concolic = true
        case "-verbose" =>
          Log.defaultLevel = Log.Level.Verbose
        case s: String =>
          if (i == args.length - 1 && options.source != null)
            options.out = new File(s)
          else if (i == args.length - 1 && options.source == null)
            options.source = new File(s)
          else if (i == args.length - 2)
            options.source = new File(s)
          else {
            log.error(s"Unrecognised option $s")
            printUsage()
            System.exit(1)
          }
      }
      i += 1
    }

    if (!options.check()) {
      printUsage()
      sys.exit(1)
    }

    val sources = if (options.source.isDirectory) {
      options.source.listFiles(new FileFilter {
        def accept(fl: File): Boolean = fl.getName.endsWith(".tip")
      })
    } else {
      Array(options.source)
    }

    sources.foreach { file =>
      log.info(s"Processing ${file.getName}")
      runWithOptions(file, options)
    }
  }

  def runWithOptions(file: File, options: RunOption) = {
    try {
      val program = Source.fromFile(file).mkString
      val tipParser = new TipParser(program)
      val res = tipParser.InputLine.run()

      res match {
        case Failure(e: ParseError) =>
          log.warn(s"Failure parsing the program: $file\n$program\n${tipParser.formatError(e, new ErrorFormatter(showTraces = true))}")
        case Failure(e: Throwable) =>
          log.warn(s"Failure parsing the program: $file\n$program", e)
        case Success(programNode : AProgram) =>
          new DeclarationAnalysis(programNode)

          if (options.cfg | options.dfAnalysis.exists(p => p._2 != dfo.Disabled && !dfo.intraprocedural(p._2))) {
            val wcfg = IntraproceduralCFG.generateFromProgram(programNode)

            if (options.cfg) {
              Utils.output(file, "", OtherOutput(OutputKindE.Cfg), wcfg.toDot({ x => x.toString }, Utils.dotIder), options.out)
            }

            options.dfAnalysis.foreach { case (s: dfa.Value, v: dfo.Value) =>
              if (!dfo.intraprocedural(v)) {
                FlowSensitiveAnalysis.select(s, v, wcfg).map { an =>
                  val res = an.analyze()
                  Utils.output(file, "", DataFlowOutput(s), wcfg.toDot(Utils.labeler(res), Utils.dotIder), options.out)
                }
              }
            }
          }
          if (options.icfg | options.dfAnalysis.exists(p => p._2 != dfo.Disabled && dfo.intraprocedural(p._2))) {
            val wcfg = ProgramCFG.generateFromProgram(programNode)

            if (options.icfg)
              Utils.output(file, "", OtherOutput(OutputKindE.Icfg), wcfg.toDot({ x => x.toString }, Utils.dotIder), options.out)

            options.dfAnalysis.foreach { case (s: dfa.Value, v: dfo.Value) =>
              if (dfo.intraprocedural(v)) {
                FlowSensitiveAnalysis.select(s, v, wcfg).map { an =>
                  val res = an.analyze()
                  Utils.output(file, "", DataFlowOutput(s), wcfg.toDot(Utils.labeler(res), Utils.dotIder), options.out)
                }
              }
            }
          }
          if (options.types) {
            val ta = TypeAnalysis(programNode)
            val ttip = programNode.toTypedString
            Utils.output(file, "", OtherOutput(OutputKindE.Ast), ttip, options.out)
            val cnst = ta.generatedConstraints()
            val cnstOut = cnst.mkString("\n")
            Utils.output(file, "", OtherOutput(OutputKindE.Constraints), cnstOut, options.out)
          }
          if (options.andersen) {
            new AndersenAnalysis(programNode)
          }
          if (options.steensgaard) {
            new SteensgaardAnalysis(programNode)
          }
          if (options.cfa) {
            new ControlFlowAnalysis(programNode)
          }
          if (options.run) {
            new Interpreter(programNode).run()
          }
          if (options.concolic) {
            new SymbolicInterpreter(programNode).test()
          }
          log.info("Success")
      }
    } catch {
      case e: Exception =>
        log.error(s"Error processing $file\n", e)
    }
  }
}
