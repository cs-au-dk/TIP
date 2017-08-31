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
  def assertContainsProgram(prog: AProgram) = {
    visit(prog, null)
  }

  /**
    * Throws an exception if the AST node `n` is not in the sub-language.
    */
  def assertContainsNode(n: AstNode) = {
    visit(n, null)
  }

  def LanguageRestrictionViolation(message: String): Nothing =
    throw new IllegalArgumentException(s"The TIP program is required to be in the ${this.getClass} sub-language.\n   $message")
}

/**
  * In this sub-language, function identifiers can only be used in direct calls, and indirect calls are prohibited.
  */
case class NoFunctionPointers(implicit declData: DeclarationData) extends TipSublanguages {

  def visit(ast: AstNode, x: Any): Unit = {
    ast match {
      case ACallFuncExpr(id: AIdentifier, args, _) =>
        id.declaration match {
          case _: AFunDeclaration =>
          case _ => LanguageRestrictionViolation(s"Identifier $id is not a function identifier")
        }
        args.foreach(visit(_, x))
      case c: ACallFuncExpr => LanguageRestrictionViolation(s"Call of the form $c is not allowed")
      case id: AIdentifier =>
        id.declaration match {
          case _: AFunDeclaration =>
            LanguageRestrictionViolation(s"Identifier $id is a function identifier not appearing in a direct call expression")
          case _ =>
        }
      case _ => visitChildren(ast, x)
    }
  }
}

/**
  * In this sub-language, the only allowed statements are the following:
  *
  * id = alloc
  * id1 = &id2
  * id1 = id2
  * id1 = *id2
  * *id1 = id2
  * id = null
  */
object NormalizedForPointsToAnalysis extends TipSublanguages {

  def visit(ast: AstNode, x: Any): Unit = {
    ast match {
      case AAssignStmt(Left(_), _: AAlloc, _) =>
      case AAssignStmt(Left(_), AUnaryOp(RefOp, _: AIdentifier, _), _) =>
      case AAssignStmt(Left(_), _: AIdentifier, _) =>
      case AAssignStmt(Left(_), AUnaryOp(DerefOp, _: AIdentifier, _), _) =>
      case AAssignStmt(Right(AUnaryOp(_, _: AIdentifier, _)), _: AIdentifier, _) =>
      case AAssignStmt(Left(_), _: ANull, _) =>
      case _: ABlock =>
      case _: AVarStmt =>
      case x: AStmt =>
        LanguageRestrictionViolation(s"Statement $x is not allowed")
      case _ => visitChildren(ast, x)
    }
  }
}

/**
  * In this sub-language, no pointers are allowed.
  */
object NoPointers extends TipSublanguages {

  def visit(ast: AstNode, x: Any): Unit = {
    ast match {
      case un: AUnaryOp[_] =>
        LanguageRestrictionViolation(s"Pointer operation $un is not allowed")
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
case class NormalizedCalls(implicit declData: DeclarationData) extends TipSublanguages {

  def visit(ast: AstNode, x: Any): Unit = {
    ast match {
      case AAssignStmt(Left(_), ACallFuncExpr(f: AIdentifier, args, _), _) =>
        if (args.exists(!_.isInstanceOf[AAtomicExpr]))
          LanguageRestrictionViolation(s"One of the arguments $args is not atomic")
        if (!f.declaration.isInstanceOf[AFunDeclaration])
          LanguageRestrictionViolation(s"Call with non-function identifier $f")
      case call: ACallFuncExpr => LanguageRestrictionViolation(s"Call $call outside an assignment is not allowed")
      case _ => visitChildren(ast, x)
    }
  }
}
