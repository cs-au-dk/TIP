package tip.ast

import tip.graph._

import scala.language.implicitConversions

trait LanguageRestriction {

  def checkCfgLanguageRestriction(cfg: ControlFlowGraph[AstNode]): Boolean

  def checkCfgNodeLanguageRestriction(nd: GNode[AstNode]): Boolean

  def checkAstLanguageRestriction(ast: AstNode): Boolean

}

/**
 * Pointer normalised
 */
object PointerNormalised extends LanguageRestriction {

  def checkCfgLanguageRestriction(cfg: ControlFlowGraph[AstNode]): Boolean = true //TODO: Implement

  def checkCfgNodeLanguageRestriction(nd: GNode[AstNode]): Boolean = true //TODO: Implement

  def checkAstLanguageRestriction(ast: AstNode): Boolean = true //TODO: Implement

  def LanguageRestrictionDoesNotApply(x: Any) = throw new IllegalArgumentException(s"PointerNormalised does not apply to $x")

  def LanguageRestrictionViolation(x: Any) = throw new IllegalArgumentException(s"PointerNormalised violated by $x")

  implicit class LeftOfAssigmentId(x: AAssignStmt) {
    def leftId: AIdentifier = {
      x.left match {
        case id: AIdentifier => id
        case _ => LanguageRestrictionViolation(x)
      }
    }
  }

}


/**
 * No pointers.
 */
object NoPointer extends LanguageRestriction {

  def checkCfgLanguageRestriction(cfg: ControlFlowGraph[AstNode]): Boolean = true //TODO: Implement

  def checkCfgNodeLanguageRestriction(nd: GNode[AstNode]): Boolean = true //TODO: Implement

  def checkAstLanguageRestriction(ast: AstNode): Boolean = true //TODO: Implement

  def LanguageRestrictionDoesNotApply(x: Any) = throw new IllegalArgumentException(s"NoPointer does not apply to $x")

  def LanguageRestrictionViolation(x: Any) = throw new IllegalArgumentException(s"NoPointer violated by $x")

  implicit class LeftOfAssigmentId(x: AAssignStmt) {
    def leftId: AIdentifier = {
      x.left match {
        case id: AIdentifier => id
        case _ => LanguageRestrictionViolation(x)
      }
    }
  }

}

/**
 * No call and return nodes.
 */
object OnlyRealnodes extends LanguageRestriction {

  def checkCfgLanguageRestriction(cfg: ControlFlowGraph[AstNode]): Boolean = {
    cfg.nodes.forall { nd =>
      checkCfgNodeLanguageRestriction(nd)
    }
  }

  def checkCfgNodeLanguageRestriction(nd: GNode[AstNode]): Boolean = {
    nd match {
      case real: GRealNode[AstNode] => true
      case entry: FunEntry[AstNode] => true
      case exit: FunExit[AstNode] => true
      case call: CallNode[AstNode] => false
      case afterCall: AfterCallNode[AstNode] => false
      case aux: AuxNode[AstNode] => false
      case n => LanguageRestrictionViolation(n)
    }
  }

  def checkAstLanguageRestriction(ast: AstNode): Boolean = {
    true
  }

  def LanguageRestrictionDoesNotApply(x: Any) = throw new IllegalArgumentException(s"OnlyRealnodes does not apply to $x")

  def LanguageRestrictionViolation(x: Any) = throw new IllegalArgumentException(s"OnlyRealnodes violated by $x")
}

/**
 * Calls can only appear in call nodes.
 */
object CallOnlyInCallNodesLanguageRestriction extends LanguageRestriction {

  def checkCfgLanguageRestriction(cfg: ControlFlowGraph[AstNode]): Boolean = {
    cfg.nodes.forall { nd =>
      checkCfgNodeLanguageRestriction(nd)
    }
  }

  def checkCfgNodeLanguageRestriction(nd: GNode[AstNode]): Boolean = {
    import AstOps._
    nd match {
      case real: GRealNode[AstNode] => !real.data.containsInvocation
      case entry: FunEntry[AstNode] => true
      case exit: FunExit[AstNode] => true
      case call: CallNode[AstNode] => call.data.containsInvocation
      case afterCall: AfterCallNode[AstNode] => afterCall.data.containsInvocation
      case n => LanguageRestrictionViolation(n)
    }
  }

  def checkAstLanguageRestriction(ast: AstNode): Boolean = {
    true
  }

  def LanguageRestrictionDoesNotApply(x: Any) = throw new IllegalArgumentException(s"CallOnlyInCallNodesLanguageRestriction does not apply to $x")

  def LanguageRestrictionViolation(x: Any) = throw new IllegalArgumentException(s"CallOnlyInCallNodesLanguageRestriction violated by $x")
}


/**
 * All the call sites are of the form
 * x = f(..)
 *
 * Under this restriction we have new, more refined operations on the cfg and ast nodes
 */
object NormalisedCallsLanguageRestrictions extends LanguageRestriction {

  override def checkCfgLanguageRestriction(cfg: ControlFlowGraph[AstNode]): Boolean = {

    CallOnlyInCallNodesLanguageRestriction.checkCfgLanguageRestriction(cfg)
    cfg.nodes.forall { nd =>
      checkCfgNodeLanguageRestriction(nd)
    }
  }

  def checkCfgNodeLanguageRestriction(nd: GNode[AstNode]): Boolean = {
    import AstOps._
    nd match {
      case real: GRealNode[AstNode] => true //We don't have calls here
      case entry: FunEntry[AstNode] => true //We don't have calls here
      case exit: FunExit[AstNode] => true //We don't have calls here
      case call: CallNode[AstNode] => call.data.containsInvocation && checkAstLanguageRestriction(call.data)
      case afterCall: AfterCallNode[AstNode] => afterCall.data.containsInvocation && checkAstLanguageRestriction(afterCall.data)
      case n => LanguageRestrictionViolation(n)
    }
  }

  override def checkAstLanguageRestriction(ast: AstNode): Boolean = {
    import AstOps._
    ast match {
      case AAssignStmt(id: AIdentifier, call: ACallFuncExpr, _) => true
      case AAssignStmt(_, call: ACallFuncExpr, _) => false
      case x => !x.containsInvocation
    }
  }

  def LanguageRestrictionDoesNotApply(x: Any) = throw new IllegalArgumentException(s"NormalisedCallsLanguageRestrictions does not apply to $x")

  def LanguageRestrictionViolation(x: Any) = throw new IllegalArgumentException(s"NormalisedCallsLanguageRestrictions violated by $x")


  implicit class CallNodeContainsAssigment(nd: CallNode[AstNode]) {

    def assignment(): ConstrainedAssigmentsContainsIdOnTheLeft = {
      nd.data match {
        case ass: AAssignStmt => ConstrainedAssigmentsContainsIdOnTheLeft(ass)
        case _ => LanguageRestrictionViolation(nd)
      }
    }

    def invocation: ACallFuncExpr = {
      this.assignment().right match {
        case call: ACallFuncExpr =>
          call
        case _ => LanguageRestrictionViolation(this.assignment())
      }
    }
  }

  implicit class AfterCallNodeContainsAssigment(nd: AfterCallNode[AstNode]) {

    def assignment(): ConstrainedAssigmentsContainsIdOnTheLeft = {
      nd.data match {
        case ass: AAssignStmt => ConstrainedAssigmentsContainsIdOnTheLeft(ass)
        case _ => LanguageRestrictionViolation(nd)
      }
    }

    def invocation: ACallFuncExpr = {
      this.assignment().right match {
        case call: ACallFuncExpr =>
          call
        case _ => LanguageRestrictionViolation(this.assignment())
      }
    }
  }

  case class ConstrainedAssigmentsContainsIdOnTheLeft(ass: AAssignStmt) {
    def leftId(): AIdentifier = {
      ass.left match {
        case id: AIdentifier => id
        case _ => LanguageRestrictionViolation(ass)
      }
    }
  }

  implicit def constrainedAssignmentToAssignment(ass: ConstrainedAssigmentsContainsIdOnTheLeft): AAssignStmt = ass.ass

}