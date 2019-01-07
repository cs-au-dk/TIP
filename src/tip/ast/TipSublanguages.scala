package tip.ast

import tip.ast.AstNodeData.DeclarationData
import AstNodeData._

/**
  * Defines restrictions of TIP for the different analyses.
  */
trait TipSublanguages extends DepthFirstAstVisitor[Any] {

  /**
    * Throws an exception if `prog` is not in the sub-language.
    */
  def assertContainsProgram(prog: AProgram): Unit =
    visit(prog, null)

  /**
    * Throws an exception if the AST node `n` is not in the sub-language.
    */
  def assertContainsNode(n: AstNode): Unit =
    visit(n, null)

  def LanguageRestrictionViolation(message: String): Nothing =
    throw new IllegalArgumentException(s"The TIP program is required to be in the ${this.getClass} sub-language\n   $message")
}

/**
  * In this sub-language, function identifiers can only be used in direct calls, and indirect calls are prohibited.
  */
class NoFunctionPointers(implicit declData: DeclarationData) extends TipSublanguages {

  def visit(ast: AstNode, x: Any): Unit =
    ast match {
      case ACallFuncExpr(targetFun: AIdentifier, args, _) =>
        targetFun.declaration match {
          case _: AFunDeclaration =>
          case _ =>
            LanguageRestrictionViolation(s"Indirect call not allowed, $targetFun is not a function")
        }
        args.foreach(visit(_, x))
      case ACallFuncExpr(targetFun, args, _) =>
        LanguageRestrictionViolation(s"Indirect call not allowed, $targetFun is not a function")
      case id: AIdentifier =>
        id.declaration match {
          case _: AFunDeclaration =>
            LanguageRestrictionViolation(s"Identifier $id is a function identifier not appearing in a direct call expression")
          case _ =>
        }
      case _ => visitChildren(ast, x)
    }
}

/**
  * In this sub-language, the only allowed statements are the following:
  *
  * id = alloc P where P is null or an integer constant
  * id1 = &id2
  * id1 = id2
  * id1 = *id2
  * *id1 = id2
  * id = null
  */
object NormalizedForPointsToAnalysis extends TipSublanguages {

  def visit(ast: AstNode, x: Any): Unit = {
    ast match {
      case AAssignStmt(_: AIdentifier, AAlloc(ANull(_) | ANumber(_, _), _), _) =>
      case AAssignStmt(_: AIdentifier, AUnaryOp(RefOp, _: AIdentifier, _), _) =>
      case AAssignStmt(_: AIdentifier, _: AIdentifier, _) =>
      case AAssignStmt(_: AIdentifier, AUnaryOp(DerefOp, _: AIdentifier, _), _) =>
      case AAssignStmt(AUnaryOp(_, _: AIdentifier, _), _: AIdentifier, _) =>
      case AAssignStmt(_: AIdentifier, _: ANull, _) =>
      case AAssignStmt(_: AIdentifier, _: AAtomicExpr, _) =>
      case _: ABlock =>
      case _: AVarStmt =>
      case _: AReturnStmt =>
      case _: AIfStmt =>
      case x: AStmt =>
        LanguageRestrictionViolation(s"Statement $x is not allowed")
      case _ =>
    }
    visitChildren(ast, x)
  }
}

/**
  * In this sub-language, no pointers are allowed.
  */
object NoPointers extends TipSublanguages {

  def visit(ast: AstNode, x: Any): Unit = {
    ast match {
      case AUnaryOp(deref: DerefOp.type, _, _) =>
        LanguageRestrictionViolation(s"Pointer operation $deref is not allowed")
      case AUnaryOp(ref: RefOp.type, _, _) =>
        LanguageRestrictionViolation(s"Pointer operation $ref is not allowed")
      case _ =>
    }
    visitChildren(ast, x)
  }
}

/**
  * In this sub-language, no calls are allowed.
  */
object NoCalls extends TipSublanguages {

  def visit(ast: AstNode, x: Any): Unit = {
    ast match {
      case call: ACallFuncExpr =>
        LanguageRestrictionViolation(s"Call $call is not allowed")
      case _ =>
    }
    visitChildren(ast, x)
  }
}

/**
  * In this sub-language, all calls are normalized, i.e. they only appear in statements of the form
  *
  * `x = f(e1, ..., en)`
  *
  * where `f` is a function identifier, `x` is a variable identifier
  * and the parameters `e_i` are atomic expressions.
  */
class NormalizedCalls(implicit declData: DeclarationData) extends TipSublanguages {

  def visit(ast: AstNode, x: Any): Unit =
    ast match {
      case AAssignStmt(_: AIdentifier, ACallFuncExpr(targetFun: AIdentifier, args, _), _) =>
        targetFun.declaration match {
          case _: AFunDeclaration =>
          case _ =>
            LanguageRestrictionViolation(s"Indirect call not allowed, $targetFun is not a function")
        }
        if (args.exists(!_.isInstanceOf[AAtomicExpr]))
          LanguageRestrictionViolation(s"One of the arguments $args is not atomic")
      case AAssignStmt(_: AIdentifier, ACallFuncExpr(targetFun, args, _), _) =>
        LanguageRestrictionViolation(s"Indirect call not allowed, $targetFun is not a function")
      case call: ACallFuncExpr => LanguageRestrictionViolation(s"Call $call outside an assignment is not allowed")
      case _ => visitChildren(ast, x)
    }
}

/**
  * This sub-language has no records and record accesses.
  */
object NoRecords extends TipSublanguages {
  def visit(ast: AstNode, x: Any): Unit = {
    ast match {
      case _: ARecord =>
        LanguageRestrictionViolation(s"Using records at: ${ast.loc}")
      case _: AAccess =>
        LanguageRestrictionViolation(s"Record access at: ${ast.loc}")
      case _ =>
    }
    visitChildren(ast, x)
  }
}
