package tip.graph

import tip.ast.AstOps._
import tip.ast._

import scala.language.implicitConversions


object NodeOps {

  /**
   * Operations that can be performed on a node [[GNode]]
   * of a control flow graph.
   */
  implicit class CfgAstOps(n: GNode[AstNode]) {

    /**
     * Returns the set of identifiers declared by the node
     */
    def declaredIds: Set[AIdentifier] = {
      n match {
        case r: GRealNode[AstNode] =>
          r.data.declaredLocals
        case _ => Set()
      }
    }

    /**
     * Returns the set of identifiers declared by the node,
     * including the parameters declaration
     */
    def declaredIdsIncludingParams: Set[AIdentifier] = {
      n match {
        case r: GRealNode[AstNode] =>
          r.data.declaredLocals
        case r: FunEntry[AstNode] =>
          r.data.asInstanceOf[AFunDeclaration].args.toSet
        case _ => Set()
      }
    }

    /**
     * Returns the set of identifiers that appear in the node
     */
    def appearingIds: Set[AIdentifier] = {
      n match {
        case r: GRealNode[AstNode] =>
          r.data.appearingIds
        case _ => Set()
      }
    }

    /**
     * Returns the set of expressions that appear in the node
     */
    def appearingExpressions: Set[AExpr] = {
      n match {
        case r: GRealNode[AstNode] =>
          r.data.appearingExpressions
        case _ => Set()
      }
    }

    /**
     * Returns the assignment that appear in the node,
     * if any
     */
    def appearingAssignments: Option[AAssignStmt] = {
      n match {
        case r: GRealNode[AstNode] =>
          r.data match {
            case ass: AAssignStmt => Some(ass)
            case _ => None
          }
        case _ => None
      }
    }

    /**
     * Returns the set of constants appearing in the node,
     * if any
     */
    def constants: Set[ANumber] = {
      n match {
        case r: GRealNode[AstNode] =>
          r.data.constants
        case _ => Set()
      }
    }

  }

}