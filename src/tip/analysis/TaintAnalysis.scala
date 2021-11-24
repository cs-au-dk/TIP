package tip.analysis

import tip.ast._
import tip.ast.AstNodeData.{AstNodeWithDeclaration, DeclarationData}
import tip.cfg._
import tip.lattices._
import tip.solvers._

/**
  * Micro-transfer-functions for taint analysis.
  */
trait TaintAnalysisFunctions extends IDEAnalysis[ADeclaration, TwoElementLattice] {

  NoPointers.assertContainsProgram(cfg.prog)
  NoRecords.assertContainsProgram(cfg.prog)

  implicit val declData: DeclarationData

  val valuelattice = new TwoElementLattice()

  val edgelattice = new EdgeFunctionLattice(valuelattice)

  import cfg._
  import edgelattice._

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
      // all other kinds of nodes: like no-ops
      case _ => Map(d -> IdEdge())
    }

  /**
    * Micro-transfer-functions for assigning an expression to an identifier.
    */
  private def assign(d: DL, id: ADeclaration, exp: AExprOrIdentifierDeclaration): Map[DL, edgelattice.EdgeFunction] = ??? //<--- Complete here
}

/**
  * Taint analysis using IDE solver.
  */
class TaintIDEAnalysis(cfg: InterproceduralProgramCfg)(implicit val declData: DeclarationData)
    extends IDESolver[ADeclaration, TwoElementLattice](cfg)
    with TaintAnalysisFunctions

/**
  * Taint analysis using summary solver.
  */
class TaintSummaryAnalysis(cfg: InterproceduralProgramCfg)(implicit val declData: DeclarationData)
    extends SummarySolver[ADeclaration, TwoElementLattice](cfg)
    with TaintAnalysisFunctions
