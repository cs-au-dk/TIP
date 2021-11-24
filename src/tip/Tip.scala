package tip

import java.io.{File, FileFilter}

import org.parboiled2.ParseError
import tip.analysis.FlowSensitiveAnalysis.{Analysis => dfa, AnalysisOption => dfo}
import tip.analysis._
import tip.ast.AstNodeData._
import tip.ast.{AProgram, NoNormalizer}
import tip.concolic.ConcolicEngine
import tip.cfg._
import tip.interpreter.ConcreteInterpreter
import tip.parser.TipParser
import tip.util._

import scala.io.Source
import scala.util.{Failure, Success}

/**
  * Options for running the TIP system.
  */
class RunOption {

  /**
    * If set, construct the (intraprocedural) control-flow graph after parsing.
    */
  var cfg = false

  /**
    * If set, construct the interprocedural control-flow graph after parsing.
    */
  var icfg = false

  /**
    * If set, perform type analysis.
    */
  var types = false

  /**
    * If set, perform control-flow analysis.
    */
  var cfa = false

  /**
    * If set, perform Andersen-style pointer analysis.
    */
  var andersen = false

  /**
    * If set, perform Steensgaard-style pointer analysis.
    */
  var steensgaard = false

  var dfAnalysis = Map[dfa.Value, dfo.Value]().withDefaultValue(dfo.Disabled)

  /**
    * Source file, or directory containing .tip files.
    */
  var source: File = _

  /**
    * Output directory. (Default: "./out")
    */
  var out: File = new File("./out")

  /**
    * If set, execute the program.
    */
  var run = false

  /**
    * If set, perform concolic execution of the program.
    */
  var concolic = false

  var normalizer: tip.ast.Normalizer = tip.ast.NoNormalizer

  /**
    * Checks that a source file or directory has been provided.
    * @return true if success
    */
  def check(): Boolean =
    if (source == null) {
      Tip.log.error(s"Source file/directory missing")
      false
    } else
      true
}

/**
  * Command-line entry for the TIP system.
  */
object Tip extends App {

  val log = Log.logger[this.type]()

  def printUsage() =
    print("""
        | Usage:
        | tip <options> <source> [out]
        |
        | <source> can be a file or a directory,
        |
        | [out] is an output directory (default: ./out)
        |
        | Options for analyzing programs:
        |
        | -types             enable type analysis
        | -cfa               enable control-flow analysis (interprocedural analyses use the call-graph obtained by this analysis)
        | -andersen          enable Andersen pointer analysis
        | -steensgaard       enable Steensgaard pointer analysis
        | -sign              enable sign analysis
        | -livevars          enable live variables analysis
        | -available         enable available expressions analysis
        | -vbusy             enable very busy expressions analysis
        | -reaching          enable reaching definitions analysis
        | -constprop         enable constant propagation analysis
        | -interval          enable interval analysis
        | -copyconstprop     enable copy constant propagation analysis
        | -uninitvars        enable possibly-uninitialized variables analysis
        | -taint             enable taint analysis
        |
        | For the dataflow analyses, the choice of fixpoint solver can be chosen by these modifiers
        | immediately after the analysis name (default: use the simple fixpoint solver):
        |
        | wl       use the worklist solver
        | wlr      use the worklist solver with reachability
        | wlrw     use the worklist solver with reachability and widening
        | wlrwn    use the worklist solver with reachability, widening, and narrowing
        | wlrp     use the worklist solver with reachability and propagation
        | iwlr     use the worklist solver with reachability, interprocedural version
        | iwlrp    use the worklist solver with reachability and propagation, interprocedural version
        | csiwlrp  use the worklist solver with reachability and propagation, context-sensitive (with call string) interprocedural version
        | cfiwlrp  use the worklist solver with reachability and propagation, context-sensitive (with functional approach) interprocedural version
        | ide      use the IDE solver
        | summary  use the summary solver
        |
        | e.g. -sign wl  will run the sign analysis using the basic worklist solver
        |
        | Options for running programs:
        |
        | -run               run the program as the last step
        | -concolic          perform concolic testing (search for failing inputs using dynamic symbolic execution)
        |
        | Options for normalizing programs (can be combined):
        |
        | -normalizereturns  normalize return statements
        | -normalizecalls    normalize function calls
        | -normalizepointers normalize pointer usages
        |
        | Other options:
        |
        | -cfg               construct the (intraprocedural) control-flow graph, but do not perform any analysis
        | -icfg              construct the interprocedural control-flow graph, but do not perform any analysis
        | -verbose           verbose output
      """.stripMargin)

  /**
    * Process the given file according to the specified options.
    */
  def processFile(file: File, options: RunOption) = {
    try {
      val program = {
        val bs = Source.fromFile(file)
        try {
          bs.mkString
        } finally {
          bs.close()
        }
      }

      // parse the program
      log.verb("Parsing")
      val tipParser = new TipParser(program)
      val res = tipParser.InputLine.run()

      res match {
        case Failure(e: ParseError) =>
          log.error(s"Failure parsing the program: $file\n${tipParser.formatError(e)}")
          sys.exit(1)
        case Failure(e: Throwable) =>
          log.error(s"Failure parsing the program: $file", e)
          sys.exit(1)
        case Success(parsedNode: AProgram) =>
          val programNode =
            if (options.normalizer == NoNormalizer) parsedNode
            else {
              // run normalizer
              log.verb("Normalizing")
              val p = options.normalizer.normalizeProgram(parsedNode)
              Output.output(file, OtherOutput(OutputKindE.normalized), p.toString, options.out)
              p
            }

          // run declaration analysis
          // (for information about the use of 'implicit', see [[tip.analysis.TypeAnalysis]])
          log.verb("Declaration analysis")
          implicit val declData: DeclarationData = new DeclarationAnalysis(programNode).analyze()

          // run selected intraprocedural flow-sensitive analyses
          if (options.cfg | options.dfAnalysis.exists(p => p._2 != dfo.Disabled && !dfo.interprocedural(p._2))) {

            // generate control-flow graph
            log.verb("Building intraprocedural control flow graphs")
            val wcfg = IntraproceduralProgramCfg.generateFromProgram(programNode)
            if (options.cfg)
              Output.output(file, OtherOutput(OutputKindE.cfg), wcfg.toDot({ x =>
                x.toString
              }, Output.dotIder), options.out)

            options.dfAnalysis.foreach {
              case (s: dfa.Value, v: dfo.Value) =>
                if (!dfo.interprocedural(v)) {
                  FlowSensitiveAnalysis.select(s, v, wcfg).foreach { an =>
                    // run the analysis
                    log.verb(s"Performing ${an.getClass.getSimpleName}")
                    val res = an.analyze().asInstanceOf[Map[CfgNode, _]]
                    Output.output(file, DataFlowOutput(s), wcfg.toDot(Output.labeler(res, an.stateAfterNode), Output.dotIder), options.out)
                  }
                }
            }
          }

          // run selected interprocedural flow-sensitive analyses
          if (options.icfg | options.dfAnalysis.exists(p => p._2 != dfo.Disabled && dfo.interprocedural(p._2))) {

            // generate control-flow graph
            val wcfg = if (options.cfa) {
              log.verb("Building interprocedural control flow graph using control flow analysis")
              InterproceduralProgramCfg.generateFromProgramWithCfa(programNode)
            } else {
              log.verb("Building interprocedural control flow graph")
              InterproceduralProgramCfg.generateFromProgram(programNode)
            }

            if (options.icfg) {
              Output.output(file, OtherOutput(OutputKindE.icfg), wcfg.toDot({ x =>
                x.toString
              }, Output.dotIder), options.out)
            }

            options.dfAnalysis.foreach {
              case (s: dfa.Value, v: dfo.Value) =>
                if (dfo.interprocedural(v)) {
                  FlowSensitiveAnalysis.select(s, v, wcfg).foreach { an =>
                    // run the analysis
                    log.verb(s"Starting ${an.getClass.getSimpleName}")
                    val res = an.analyze()
                    val res2 =
                      if (dfo.contextsensitive(v))
                        Output.transform(res.asInstanceOf[Map[(CallContext, CfgNode), _]])
                      else
                        res.asInstanceOf[Map[CfgNode, _]]
                    Output.output(file, DataFlowOutput(s), wcfg.toDot(Output.labeler(res2, an.stateAfterNode), Output.dotIder), options.out)
                  }
                }
            }
          }

          // run type analysis, if selected
          if (options.types) {
            // (for information about the use of 'implicit', see [[tip.analysis.TypeAnalysis]])
            log.verb("Starting TypeAnalysis")
            implicit val typeData: TypeData = new TypeAnalysis(programNode).analyze()
            Output.output(file, OtherOutput(OutputKindE.types), programNode.toTypedString, options.out)
          }

          // run Andersen analysis, if selected
          if (options.andersen) {
            log.verb("Starting AndersenAnalysis")
            val s = new AndersenAnalysis(programNode)
            s.analyze()
            s.pointsTo()
          }

          // run Steensgaard analysis, if selected
          if (options.steensgaard) {
            log.verb("Starting SteensgaardAnalysis")
            val s = new SteensgaardAnalysis(programNode)
            s.analyze()
            s.pointsTo()
          }

          // run control-flow analysis, if selected
          if (options.cfa) { // TODO: skip if InterproceduralProgramCfg.generateFromProgramWithCfa has been executed above
            val s = new ControlFlowAnalysis(programNode)
            log.verb("Starting ControlFlowAnalysis")
            s.analyze()
          }

          // execute the program, if selected
          if (options.run) {
            log.verb("Starting ConcreteInterpreter")
            val intp = new ConcreteInterpreter(programNode)
            intp.semp()
          }

          // concolically execute the program, if selected
          if (options.concolic) {
            log.verb("Starting ConcolicEngine")
            new ConcolicEngine(programNode).test()
          }
      }
    } catch {
      case e: TipProgramException =>
        log.error(e.getMessage)
        sys.exit(1)
      case e: Exception =>
        log.error(s"Internal error: ${e.getMessage}", e)
        sys.exit(1)
    }
  }

  // parse options
  val options = new RunOption()
  var i = 0
  while (i < args.length) {
    val s = args(i)
    if (s.head == '-')
      s match {
        case "-normalizepointers" =>
          options.normalizer = new tip.ast.CombineNormalizers(options.normalizer, tip.ast.PointersNormalizer)
        case "-normalizecalls" =>
          options.normalizer = new tip.ast.CombineNormalizers(options.normalizer, tip.ast.CallsNormalizer)
        case "-normalizereturns" =>
          options.normalizer = new tip.ast.CombineNormalizers(options.normalizer, tip.ast.ReturnsNormalizer)
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
        case "-sign" | "-livevars" | "-available" | "-vbusy" | "-reaching" | "-constprop" | "-interval" | "-copyconstprop" | "-uninitvars" | "-taint" =>
          options.dfAnalysis += dfa.withName(args(i).drop(1)) -> {
            if (i + 1 < args.length && dfo.values.map(_.toString()).contains(args(i + 1))) {
              i = i + 1
              dfo.withName(args(i))
            } else
              dfo.simple
          }
        case "-run" =>
          options.run = true
        case "-concolic" =>
          options.concolic = true
        case "-verbose" =>
          Log.defaultLevel = Log.Level.Verbose
          log.level = Log.Level.Verbose
        case _ =>
          log.error(s"Unrecognized option $s")
          printUsage()
          sys.exit(1)
      } else if (i == args.length - 1 && options.source != null)
      options.out = new File(s)
    else if ((i == args.length - 1 && options.source == null) || i == args.length - 2)
      options.source = new File(s)
    else {
      log.error(s"Unexpected argument $s")
      printUsage()
      sys.exit(1)
    }
    i += 1
  }
  if (!options.check()) {
    printUsage()
    sys.exit(1)
  }
  val sources = if (options.source.isDirectory) {
    // directory provided, get the .tip files
    options.source.listFiles(new FileFilter {
      def accept(fl: File): Boolean = fl.getName.endsWith(".tip")
    })
  } else {
    // single file provided
    Array(options.source)
  }
  options.out.mkdirs()

  // process each source file
  sources.foreach { file =>
    log.info(s"Processing ${file.getName}")
    processFile(file, options)
  }

}
