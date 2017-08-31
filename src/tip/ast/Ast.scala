package tip.ast
import tip.ast.AstPrinters._

object AstNode {

  var lastUid: Int = 0

  /**
    * Only `AIdentifier` and `AUnaryOp[DerefOp.type]` are legal left-hand-sides of assignments.
    * An AST node of type `Assignable` matches `Left(id)` if it is an identifier `id`, and
    * it matches `Right(AUnaryOp(DerefOp, e, _))` if it is a dereference of `e`.
    */
  type Assignable = Either[AIdentifier, AUnaryOp[DerefOp.type]]
}

/**
  * Source code location.
  */
case class Loc(line: Int, col: Int) {
  override def toString: String = s"$line:$col"
}

sealed trait Operator
sealed trait BinaryOperator
sealed trait UnaryOperator

case object Plus extends Operator with BinaryOperator {
  override def toString: String = "+"
}

case object Minus extends Operator with BinaryOperator {
  override def toString: String = "-"
}

case object Times extends Operator with BinaryOperator {
  override def toString: String = "*"
}

case object Divide extends Operator with BinaryOperator {
  override def toString: String = "/"
}

case object Eqq extends Operator with BinaryOperator {
  override def toString: String = "=="
}

case object GreatThan extends Operator with BinaryOperator {
  override def toString: String = ">"
}

case object RefOp extends Operator with UnaryOperator {
  override def toString: String = "&"
}

case object DerefOp extends Operator with UnaryOperator {
  override def toString: String = "*"
}

/**
  * AST node.
  */
sealed abstract class AstNode extends AnyRef with Product {

  /**
    * Unique ID of the node.
    * Every new node object gets a fresh ID (but the ID is ignored in equals tests).
    */
  val uid: Int = { AstNode.lastUid += 1; AstNode.lastUid }

  /**
    * Source code location.
    */
  val loc: Loc

  override def toString: String = {
    s"${this.print(PartialFunction.empty)}:$loc"
  }
}

//////////////// Expressions //////////////////////////

sealed trait AExprOrIdentifierDeclaration extends AstNode

sealed trait AExpr extends AExprOrIdentifierDeclaration

sealed trait AAtomicExpr extends AExpr

sealed trait ADeclaration extends AstNode

case class ACallFuncExpr(targetFun: AExpr, args: List[AExpr], loc: Loc) extends AExpr

case class AIdentifierDeclaration(value: String, loc: Loc) extends ADeclaration with AExprOrIdentifierDeclaration

case class AIdentifier(value: String, loc: Loc) extends AExpr with AAtomicExpr

case class ABinaryOp(operator: BinaryOperator, left: AExpr, right: AExpr, loc: Loc) extends AExpr

case class AUnaryOp[+T <: UnaryOperator](operator: T, target: AExpr, loc: Loc) extends AExpr

case class ANumber(value: Int, loc: Loc) extends AExpr with AAtomicExpr

case class AInput(loc: Loc) extends AExpr with AAtomicExpr

case class AAlloc(loc: Loc) extends AExpr with AAtomicExpr

case class ANull(loc: Loc) extends AExpr with AAtomicExpr

//////////////// Statements //////////////////////////

sealed trait AStmt extends AstNode

/**
  * A statement in the body of a nested block (cannot be a declaration or a return).
  */
sealed trait AStmtInNestedBlock extends AStmt

case class AAssignStmt(left: AstNode.Assignable, right: AExpr, loc: Loc) extends AStmtInNestedBlock

sealed trait ABlock extends AStmt {

  /**
    * All the statements in the block, in order.
    */
  def body: List[AStmt]

}

case class ANestedBlockStmt(body: List[AStmtInNestedBlock], loc: Loc) extends ABlock with AStmtInNestedBlock

case class AFunBlockStmt(declarations: List[AVarStmt], others: List[AStmtInNestedBlock], ret: AReturnStmt, loc: Loc) extends ABlock {

  /**
    * The contents of the block, not partitioned into declarations, others and return
    */
  val body: List[AStmt] = declarations ++ (others :+ ret)
}

case class AIfStmt(guard: AExpr, ifBranch: AStmtInNestedBlock, elseBranch: Option[AStmtInNestedBlock], loc: Loc) extends AStmtInNestedBlock

case class AOutputStmt(value: AExpr, loc: Loc) extends AStmtInNestedBlock

case class AReturnStmt(value: AExpr, loc: Loc) extends AStmt

case class AErrorStmt(value: AExpr, loc: Loc) extends AStmtInNestedBlock

case class AVarStmt(declIds: List[AIdentifierDeclaration], loc: Loc) extends AStmt

case class AWhileStmt(guard: AExpr, innerBlock: AStmtInNestedBlock, loc: Loc) extends AStmtInNestedBlock

//////////////// Program and function ///////////////

case class AProgram(funs: List[AFunDeclaration], loc: Loc) extends AstNode {

  def mainFunction: AFunDeclaration = {
    val main = findMainFunction()
    if (main.isDefined) main.get
    else throw new RuntimeException(s"Missing main function, declared functions are $funs")
  }

  def hasMainFunction: Boolean = {
    findMainFunction().isDefined
  }

  private def findMainFunction(): Option[AFunDeclaration] = {
    funs.find(decl => decl.name == "main")
  }

}

case class AFunDeclaration(name: String, args: List[AIdentifierDeclaration], stmts: AFunBlockStmt, loc: Loc) extends ADeclaration {
  override def toString: String = s"$name (${args.mkString(",")}){...}"
}
