package tip.cfg

import tip.ast.AstNodeData._
import tip.ast.AstOps._
import tip.ast._

object CfgOps {

  /**
    * An implicit class with convenience methods for operations on CFG nodes.
    *
    * (For information about implicit classes, see [[tip.ast.AstNodeData.AstNodeWithDeclaration]].)
    */
  implicit class CfgNodeOps(n: CfgNode) {

    /**
      * Returns the set of identifiers declared by the node, including only local variables.
      */
    def declaredVars(implicit declData: DeclarationData): Set[ADeclaration] =
      n match {
        case r: CfgStmtNode =>
          r.data.declaredLocals
        case _ => Set()
      }

    /**
      * Returns the set of identifiers declared by the node, including local variables, function parameters, and function identifiers.
      */
    def declaredVarsAndParams(implicit declData: DeclarationData): Set[ADeclaration] =
      n match {
        case r: CfgStmtNode =>
          r.data.declaredLocals
        case r: CfgFunEntryNode =>
          r.data.params.toSet + r.data
        case _ => Set()
      }

    /**
      * Returns the set of declarations of the identifiers that appear in the node.
      */
    def appearingIds(implicit declData: DeclarationData): Set[ADeclaration] =
      n match {
        case r: CfgStmtNode =>
          r.data.appearingIds
        case _ => Set()
      }

    /**
      * Returns the set of expressions that appear in the node.
      */
    def appearingNonInputExpressions: Set[AExpr] =
      n match {
        case r: CfgStmtNode =>
          r.data.appearingNonInputExpressions
        case _ => Set()
      }

    /**
      * Returns the assignment that appears in the node, if any.
      */
    def appearingAssignments: Option[AAssignStmt] =
      n match {
        case r: CfgStmtNode =>
          r.data match {
            case as: AAssignStmt => Some(as)
            case _ => None
          }
        case _ => None
      }

    /**
      * Returns the set of constants appearing in the node, if any.
      */
    def appearingConstants: Set[ANumber] =
      n match {
        case r: CfgStmtNode =>
          r.data.appearingConstants
        case _ => Set()
      }
  }
}
