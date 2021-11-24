package tip.analysis

import tip.ast._
import tip.ast.AstNodeData.{AstNodeWithDeclaration, DeclarationData}
import tip.cfg._
import tip.lattices._
import tip.solvers._

import scala.collection.mutable

/**
  * Micro-transfer-functions for copy-constant-propagation analysis.
  */
trait CopyConstantPropagationAnalysisFunctions extends IDEAnalysis[ADeclaration, FlatLattice[Int]] {

  NoPointers.assertContainsProgram(cfg.prog)
  NoRecords.assertContainsProgram(cfg.prog)

  implicit val declData: DeclarationData

  val valuelattice = new FlatLattice[Int]()

  val edgelattice: EdgeFunctionLattice[valuelattice.type] = new EdgeFunctionLattice(valuelattice)

  import cfg._
  import edgelattice._
  import edgelattice.valuelattice._

  def edgesCallToEntry(call: CfgCallNode, entry: CfgFunEntryNode)(d: DL): Map[DL, edgelattice.EdgeFunction] =
    entry.data.params.zip(call.invocation.args).foldLeft(Map[DL, edgelattice.EdgeFunction]()) {
      case (acc, (id, exp)) =>
        acc ++ assign(d, id, exp)
    }

  def edgesExitToAfterCall(exit: CfgFunExitNode, aftercall: CfgAfterCallNode)(d: DL): Map[DL, edgelattice.EdgeFunction] =
    assign(d, aftercall.targetIdentifier.declaration, AstOps.returnId)

  def edgesCallToAfterCall(call: CfgCallNode, aftercall: CfgAfterCallNode)(d: DL): Map[DL, edgelattice.EdgeFunction] =
    d match {
      case Right(_) => Map(d -> IdEdge())
      case Left(a) => if (a == aftercall.targetIdentifier.declaration) Map() else Map(d -> IdEdge())
    }

  def edgesOther(n: CfgNode)(d: DL): Map[DL, edgelattice.EdgeFunction] =
    n match {
      case r: CfgStmtNode =>
        r.data match {

          // var declarations
          case varr: AVarStmt =>
            vars(d, varr.declIds)

          // assignments
          case as: AAssignStmt =>
            as match {
              case AAssignStmt(id: AIdentifier, right, _) =>
                val edges = assign(d, id.declaration, right)
                d match {
                  case Left(a) if id.declaration != a =>
                    edges + (d -> IdEdge()) // not at the variable being written to, so add identity edge
                  case _ =>
                    edges
                }
              case AAssignStmt(_, _, _) => NoPointers.LanguageRestrictionViolation(s"$as not allowed", as.loc)
            }

          // return statement
          case ret: AReturnStmt => assign(d, AstOps.returnId, ret.exp)

          // all other kinds of statements: like no-ops
          case _ => Map(d -> IdEdge())
        }

      // the program entry is like a 'var' for the parameters
      case funentry: CfgFunEntryNode if programEntry == funentry =>
        vars(d, funentry.data.params)

      // all other kinds of nodes: like no-ops
      case _ => Map(d -> IdEdge())
    }

  /**
    * Micro-transfer-functions for assigning an expression to an identifier.
    */
  private def assign(d: DL, id: ADeclaration, exp: AExprOrIdentifierDeclaration): Map[DL, edgelattice.EdgeFunction] = {
    val edges = mutable.ListBuffer[(DL, EdgeFunction)]()
    d match {
      case Right(_) =>
        edges += d -> IdEdge() // identity edge from lambda to lambda
        exp match {
          case AIdentifier(_, _) | AIdentifierDeclaration(_, _) => // if the expression is a variable, no additional edges from lambda
          case num: ANumber => // TODO: could also handle cases like x = 1+2*3 using local constant-only constant folding
            edges += Left(id) -> ConstEdge(FlatEl(num.value)) // if the expression is a constant, add constant edge from lambda to the variable being assigned to
          case _ =>
            edges += Left(id) -> ConstEdge(Top) // for other expressions, add top edge from lambda to the variable being assigned to
        }
      case Left(a) =>
        exp match {
          case aid @ (AIdentifier(_, _) | AIdentifierDeclaration(_, _)) =>
            val aiddecl = aid match {
              case aid: AIdentifier => aid.declaration
              case aid: AIdentifierDeclaration => aid
              case _ => ??? // unreachable, aid is an AIdentifier or an AIdentifierDeclaration
            }
            if (aiddecl == a) // at the variable being read from?
              edges += Left(id) -> IdEdge() // identity edge to the variable being written to
          case _ => // ignore other kinds of expressions
        }
    }
    edges.toMap
  }

  /**
    * Micro-transfer-functions for variable declarations and parameters of the main function.
    */
  private def vars(d: DL, ids: List[AIdentifierDeclaration]): Map[DL, edgelattice.EdgeFunction] =
    d match {
      case Right(_) =>
        ids.foldLeft(Map(d -> IdEdge()): Map[DL, EdgeFunction]) { (ps, id) => // identity edge from lambda to lambda
          ps + (Left(id) -> ConstEdge(Top)) // top edge from lambda to each variable being declared
        }
      case Left(a) =>
        if (ids.contains(a))
          Map() // no edges from the variables being declared
        else
          Map(d -> IdEdge()) // identity edge from all other variables to themselves
    }
}

/**
  * Copy-constant-propagation analysis using IDE solver.
  */
class CopyConstantPropagationIDEAnalysis(cfg: InterproceduralProgramCfg)(implicit val declData: DeclarationData)
    extends IDESolver[ADeclaration, FlatLattice[Int]](cfg)
    with CopyConstantPropagationAnalysisFunctions

/**
  * Copy-constant-propagation analysis using summary solver.
  */
class CopyConstantPropagationSummaryAnalysis(cfg: InterproceduralProgramCfg)(implicit val declData: DeclarationData)
    extends SummarySolver[ADeclaration, FlatLattice[Int]](cfg)
    with CopyConstantPropagationAnalysisFunctions
