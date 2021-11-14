package tip.analysis

import tip.ast._
import tip.cfg._
import tip.lattices._
import tip.solvers._
import tip.ast.AstNodeData.{AstNodeWithDeclaration, DeclarationData}
import tip.ast.AstOps.AstOp

import scala.collection.mutable

/**
  * Micro-transfer-functions for possibly-uninitialized variables analysis.
  */
trait PossiblyUninitializedVarsAnalysisFunctions extends IDEAnalysis[ADeclaration, TwoElementLattice] {

  NoPointers.assertContainsProgram(cfg.prog)
  NoRecords.assertContainsProgram(cfg.prog)

  implicit val declData: DeclarationData

  val valuelattice = new TwoElementLattice()

  val edgelattice: EdgeFunctionLattice[valuelattice.type] = new EdgeFunctionLattice(valuelattice)

  import cfg._
  import edgelattice._
  import edgelattice.valuelattice._

  def edgesCallToEntry(d: DL, call: CfgCallNode, entry: CfgFunEntryNode): List[(DL, edgelattice.EdgeFunction)] =
    entry.data.params.zip(call.invocation.args).foldLeft(List[(DL, edgelattice.EdgeFunction)]()) {
      case (acc, (id, exp)) =>
        acc ++ assign(d, id, exp)
    }

  def edgesExitToAfterCall(d: DL, exit: CfgFunExitNode, aftercall: CfgAfterCallNode): List[(DL, edgelattice.EdgeFunction)] =
    assign(d, aftercall.targetIdentifier.declaration, AstOps.returnId)

  def edgesCallToAfterCall(d2: DL, call: CfgCallNode, aftercall: CfgAfterCallNode): List[(DL, edgelattice.EdgeFunction)] =
    d2 match {
      case Right(_) => List((d2, IdEdge()))
      case Left(a) => if (a == aftercall.targetIdentifier.declaration) List() else List((d2, IdEdge()))
    }

  def edgesOther(d: DL, n: CfgNode): List[(DL, edgelattice.EdgeFunction)] =
    n match {
      case r: CfgStmtNode =>
        r.data match {

          // var declarations
          case varr: AVarStmt =>
            d match {
              case Right(_) =>
                varr.declIds.foldLeft(List((d, IdEdge())): List[(DL, EdgeFunction)]) { (ps, id) => // identity edge from lambda to lambda
                  ps :+ (Left(id), ConstEdge(Top)) // top edge from lambda to each variable being declared
                }
              case Left(a) =>
                if (varr.declIds.contains(a))
                  List() // no edges from the variables being declared
                else
                  List((d, IdEdge())) // identity edge from all other variables to themselves
            }

          // assignments
          case as: AAssignStmt =>
            as match {
              case AAssignStmt(id: AIdentifier, right, _) =>
                val edges = assign(d, id.declaration, right)
                d match {
                  case Left(a) if id.declaration != a =>
                    edges :+ ((d, IdEdge())) // not at the variable being written to, so add identity edge
                  case _ =>
                    edges
                }
              case AAssignStmt(_, _, _) => NoPointers.LanguageRestrictionViolation(s"$as not allowed", as.loc)
            }

          // return statement
          case ret: AReturnStmt => assign(d, AstOps.returnId, ret.exp)

          // all other kinds of statements: like no-ops
          case _ => List((d, IdEdge()))
        }
      // all other kinds of nodes: like no-ops
      case _ => List((d, IdEdge()))
    }

  /**
    * Micro-transfer-functions for assigning an expression to an identifier.
    */
  private def assign(d: DL, id: ADeclaration, exp: AExprOrIdentifierDeclaration): List[(DL, edgelattice.EdgeFunction)] = {
    val edges = mutable.ListBuffer[(DL, EdgeFunction)]()
    d match {
      case Right(_) =>
        edges += ((d, IdEdge())) // identity edge from lambda to lambda
      case Left(a) =>
        // identity edge from d to the variable being assigned to if d appears in exp
        if (exp.appearingIds.contains(a))
          edges += ((Left(id), IdEdge()))
    }
    edges.toList
  }
}

/**
  * Possibly-uninitialized variables analysis using IDE solver.
  */
class PossiblyUninitializedVarsIDEAnalysis(cfg: InterproceduralProgramCfg)(implicit val declData: DeclarationData)
    extends IDESolver[ADeclaration, TwoElementLattice](cfg)
    with PossiblyUninitializedVarsAnalysisFunctions
