package tip.ast

import tip.ast.AstNodeData.DeclarationData
import AstNodeData._
import tip.util.TipProgramException

/**
  * Defines restrictions of TIP for the different analyses.
  */
trait TipSublanguages extends DepthFirstAstVisitor[Unit] {

  /**
    * Throws an exception if `prog` is not in the sub-language.
    */
  def assertContainsProgram(prog: AProgram): Unit =
    visit(prog, ())

  /**
    * Throws an exception if the AST node `n` is not in the sub-language.
    */
  def assertContainsNode(n: AstNode): Unit =
    visit(n, ())

  def LanguageRestrictionViolation(message: String, loc: Loc): Nothing =
    throw new TipProgramException(s"The analysis requires the program to be in the ${this.getClass} sub-language\n   $message ${loc.toStringLong}")
}

/**
  * In this sub-language, function identifiers can only be used in direct calls, and indirect calls are prohibited.
  */
class NoFunctionPointers(implicit declData: DeclarationData) extends TipSublanguages {

  def visit(ast: AstNode, x: Unit): Unit =
    ast match {
      case ACallFuncExpr(targetFun: AIdentifier, args, _) =>
        targetFun.declaration match {
          case _: AFunDeclaration =>
          case _ =>
            LanguageRestrictionViolation(s"Indirect call not allowed, $targetFun is not a function", ast.loc)
        }
        args.foreach(visit(_, x))
      case ACallFuncExpr(targetFun, _, _) =>
        LanguageRestrictionViolation(s"Indirect call not allowed, $targetFun is not a function", ast.loc)
      case id: AIdentifier =>
        id.declaration match {
          case _: AFunDeclaration =>
            LanguageRestrictionViolation(s"Identifier $id is a function identifier not appearing in a direct call expression", ast.loc)
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

  def visit(ast: AstNode, x: Unit): Unit = {
    ast match {
      case AAssignStmt(_: AIdentifier, AAlloc(ANull(_) | ANumber(_, _), _), _) =>
      case AAssignStmt(_: AIdentifier, AVarRef(_, _), _) =>
      case AAssignStmt(_: AIdentifier, _: AIdentifier, _) =>
      case AAssignStmt(_: AIdentifier, AUnaryOp(DerefOp, _: AIdentifier, _), _) =>
      case AAssignStmt(ADerefWrite(_: AIdentifier, _), _: AIdentifier, _) =>
      case AAssignStmt(_: AIdentifier, _: ANull, _) =>
      case AAssignStmt(_: AIdentifier, _: AAtomicExpr, _) =>
      case _: ABlock =>
      case _: AVarStmt =>
      case _: AReturnStmt =>
      case _: AIfStmt =>
      case stmt: AStmt =>
        LanguageRestrictionViolation(s"Statement $stmt is not allowed", ast.loc)
      case _ =>
    }
    visitChildren(ast, x)
  }
}

/**
  * In this sub-language, no pointers are allowed.
  */
object NoPointers extends TipSublanguages {

  def visit(ast: AstNode, x: Unit): Unit = {
    ast match {
      case AUnaryOp(_: DerefOp.type, _, _) =>
        LanguageRestrictionViolation(s"Pointer load operation is not allowed", ast.loc)
      case AVarRef(_, _) =>
        LanguageRestrictionViolation(s"Pointer reference operation is not allowed", ast.loc)
      case as: AAssignStmt =>
        as.left match {
          case _: ADerefWrite =>
            LanguageRestrictionViolation(s"Pointer store operation is not allowed", ast.loc)
          case _: AIndirectFieldWrite =>
            LanguageRestrictionViolation(s"Pointer field store operation is not allowed", ast.loc)
          case _ =>
        }
      case _ =>
    }
    visitChildren(ast, x)
  }
}

/**
  * In this sub-language, no calls are allowed.
  */
object NoCalls extends TipSublanguages {

  def visit(ast: AstNode, x: Unit): Unit = {
    ast match {
      case call: ACallFuncExpr =>
        LanguageRestrictionViolation(s"Call $call is not allowed", ast.loc)
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

  def visit(ast: AstNode, x: Unit): Unit =
    ast match {
      case AAssignStmt(_: AIdentifier, ACallFuncExpr(targetFun: AIdentifier, args, _), _) =>
        targetFun.declaration match {
          case _: AFunDeclaration =>
          case _ =>
            LanguageRestrictionViolation(s"Indirect call not allowed, $targetFun is not a function", ast.loc)
        }
        if (args.exists(!_.isInstanceOf[AAtomicExpr]))
          LanguageRestrictionViolation(s"One of the arguments $args is not atomic", ast.loc)
      case AAssignStmt(_: AIdentifier, ACallFuncExpr(targetFun, args, _), _) =>
        LanguageRestrictionViolation(s"Indirect call not allowed, $targetFun is not a function", ast.loc)
      case call: ACallFuncExpr => LanguageRestrictionViolation(s"Call $call outside an assignment is not allowed", ast.loc)
      case _ => visitChildren(ast, x)
    }
}

/**
  * This sub-language has no records and record accesses.
  */
object NoRecords extends TipSublanguages {

  def visit(ast: AstNode, x: Unit): Unit = {
    ast match {
      case _: ARecord =>
        LanguageRestrictionViolation(s"Record is not allowed", ast.loc)
      case _: AFieldAccess =>
        LanguageRestrictionViolation(s"Field read is not allowed", ast.loc)
      case as: AAssignStmt =>
        as.left match {
          case _: ADirectFieldWrite =>
            LanguageRestrictionViolation(s"Field write operation is not allowed", ast.loc)
          case _: AIndirectFieldWrite =>
            LanguageRestrictionViolation(s"Pointer field store operation is not allowed", ast.loc)
          case _ =>
        }
      case _ =>
    }
    visitChildren(ast, x)
  }
}
