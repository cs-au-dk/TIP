package tip.ast
import tip.ast.AstPrinters._
import tip.util.TipProgramException

/**
  * Source code location.
  */
case class Loc(line: Int, col: Int) {
  override def toString: String = s"$line:$col"

  def toStringLong: String = s"(line $line, column $col)"
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

case object DerefOp extends Operator with UnaryOperator {
  override def toString: String = "*"
}

/**
  * AST node.
  *
  * (The class extends `Product` to enable functionality used by [[AstOps.UnlabelledNode]].)
  */
sealed abstract class AstNode extends Product {

  /**
    * Source code location.
    */
  val loc: Loc

  override def toString: String =
    s"${this.print(PartialFunction.empty)}[$loc]"
}

//////////////// Expressions //////////////////////////

sealed trait AExprOrIdentifierDeclaration extends AstNode

sealed trait AExpr extends AExprOrIdentifierDeclaration

sealed trait AAtomicExpr extends AExpr

sealed trait ADeclaration extends AstNode

case class ACallFuncExpr(targetFun: AExpr, args: List[AExpr], loc: Loc) extends AExpr

case class AIdentifierDeclaration(name: String, loc: Loc) extends ADeclaration with AExprOrIdentifierDeclaration

case class AIdentifier(name: String, loc: Loc) extends AExpr with AAtomicExpr with ReferenceAssignable

case class ABinaryOp(operator: BinaryOperator, left: AExpr, right: AExpr, loc: Loc) extends AExpr

case class AUnaryOp(operator: UnaryOperator, subexp: AExpr, loc: Loc) extends AExpr

case class ANumber(value: Int, loc: Loc) extends AExpr with AAtomicExpr

case class AInput(loc: Loc) extends AExpr with AAtomicExpr

case class AAlloc(exp: AExpr, loc: Loc) extends AExpr with AAtomicExpr

case class AVarRef(id: AIdentifier, loc: Loc) extends AExpr with AAtomicExpr

case class ANull(loc: Loc) extends AExpr with AAtomicExpr

case class ARecord(fields: List[ARecordField], loc: Loc) extends AExpr

case class ARecordField(field: String, exp: AExpr, loc: Loc)

case class AFieldAccess(record: AExpr, field: String, loc: Loc) extends AExpr with AAtomicExpr

//////////////// Statements //////////////////////////

sealed trait AStmt extends AstNode

/**
  * A statement in the body of a nested block (cannot be a declaration or a return).
  */
sealed trait AStmtInNestedBlock extends AStmt

case class AAssignStmt(left: Assignable, right: AExpr, loc: Loc) extends AStmtInNestedBlock

sealed trait Assignable

sealed trait ReferenceAssignable extends Assignable

case class ADerefWrite(exp: AExpr, loc: Loc) extends ReferenceAssignable

sealed trait FieldAssignable extends Assignable

case class ADirectFieldWrite(id: AIdentifier, field: String, loc: Loc) extends FieldAssignable

case class AIndirectFieldWrite(exp: AExpr, field: String, loc: Loc) extends FieldAssignable

sealed trait ABlock extends AStmt {

  /**
    * All the statements in the block, in order.
    */
  def body: List[AStmt]

}

case class ANestedBlockStmt(body: List[AStmtInNestedBlock], loc: Loc) extends ABlock with AStmtInNestedBlock

case class AFunBlockStmt(declarations: List[AVarStmt], others: List[AStmtInNestedBlock], ret: AReturnStmt, loc: Loc) extends ABlock {

  /**
    * The contents of the block: declarations, others, and return.
    */
  val body: List[AStmt] = declarations ++ (others :+ ret)
}

case class AIfStmt(guard: AExpr, ifBranch: AStmtInNestedBlock, elseBranch: Option[AStmtInNestedBlock], loc: Loc) extends AStmtInNestedBlock

case class AOutputStmt(exp: AExpr, loc: Loc) extends AStmtInNestedBlock

case class AReturnStmt(exp: AExpr, loc: Loc) extends AStmt

case class AErrorStmt(exp: AExpr, loc: Loc) extends AStmtInNestedBlock

case class AVarStmt(declIds: List[AIdentifierDeclaration], loc: Loc) extends AStmt

case class AWhileStmt(guard: AExpr, innerBlock: AStmtInNestedBlock, loc: Loc) extends AStmtInNestedBlock

//////////////// Program and function ///////////////

case class AProgram(funs: List[AFunDeclaration], loc: Loc) extends AstNode {

  def mainFunction: AFunDeclaration = {
    val main = findMainFunction()
    if (main.isDefined) main.get
    else throw new TipProgramException(s"Missing main function, declared functions are $funs")
  }

  def hasMainFunction: Boolean =
    findMainFunction().isDefined

  private def findMainFunction(): Option[AFunDeclaration] =
    funs.find(decl => decl.name == "main")

  override def toString: String =
    s"${this.print(PartialFunction.empty)}"
}

case class AFunDeclaration(name: String, params: List[AIdentifierDeclaration], stmts: AFunBlockStmt, loc: Loc) extends ADeclaration {
  override def toString: String = s"$name(${params.mkString(",")}){...}:$loc"
}
