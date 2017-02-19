package tip.ast

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

object Plus extends Operator with BinaryOperator {
  override def toString: String = "+"
}

object Minus extends Operator with BinaryOperator {
  override def toString: String = "-"
}

object Times extends Operator with BinaryOperator {
  override def toString: String = "*"
}

object Divide extends Operator with BinaryOperator {
  override def toString: String = "/"
}

object Eqq extends Operator with BinaryOperator {
  override def toString: String = "=="
}

object GreatThan extends Operator with BinaryOperator {
  override def toString: String = ">"
}

object RefOp extends Operator with UnaryOperator {
  override def toString: String = "&"
}

object DerefOp extends Operator with UnaryOperator {
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
  val uid = { AstNode.lastUid += 1; AstNode.lastUid }

  /**
    * Source code location.
    */
  val loc: Loc

  def toCustomString(printer: PartialFunction[AstNode, String]): String
}

//////////////// Expressions //////////////////////////

sealed trait AExpr extends AstNode

sealed trait AAtomicExpr extends AExpr

sealed trait ADeclaration extends AstNode

case class ACallFuncExpr(targetFun: AExpr, args: List[AExpr], loc: Loc) extends AExpr {
  override def toString: String = s"${toCustomString(PartialFunction.empty)}:$loc"

  def toCustomString(printer: PartialFunction[AstNode, String]): String =
    printer.applyOrElse(this, { _: ACallFuncExpr =>
      s"${targetFun.toCustomString(printer)}(${args.map(_.toCustomString(printer)).mkString(",")})"
    })
}

case class AIdentifierDeclaration(value: String, loc: Loc) extends ADeclaration {
  override def toString: String = s"${toCustomString(PartialFunction.empty)}:$loc"

  def toCustomString(printer: PartialFunction[AstNode, String]): String =
    printer.applyOrElse(this, { _: AIdentifierDeclaration =>
      s"$value"
    })
}

case class AIdentifier(value: String, loc: Loc) extends AExpr with AAtomicExpr {
  override def toString: String = s"${toCustomString(PartialFunction.empty)}:$loc"

  def toCustomString(printer: PartialFunction[AstNode, String]): String =
    printer.applyOrElse(this, { _: AIdentifier =>
      value
    })
}

case class ABinaryOp(operator: BinaryOperator, left: AExpr, right: AExpr, loc: Loc) extends AExpr {
  override def toString: String = s"${toCustomString(PartialFunction.empty)}:$loc"

  def toCustomString(printer: PartialFunction[AstNode, String]): String =
    printer.applyOrElse(this, { _: ABinaryOp =>
      left.toCustomString(printer) + " " + operator + " " + right.toCustomString(printer)
    })
}

case class AUnaryOp[+T <: UnaryOperator](operator: T, target: AExpr, loc: Loc) extends AExpr {
  override def toString: String = s"${toCustomString(PartialFunction.empty)}:$loc"

  def toCustomString(printer: PartialFunction[AstNode, String]): String =
    printer.applyOrElse(this, { _: AUnaryOp[T] =>
      s"$operator${target.toCustomString(printer)}"
    })
}

case class ANumber(value: Int, loc: Loc) extends AExpr with AAtomicExpr {
  override def toString: String = s"${toCustomString(PartialFunction.empty)}:$loc"

  def toCustomString(printer: PartialFunction[AstNode, String]): String =
    printer.applyOrElse(this, { _: ANumber =>
      s"$value"
    })
}

case class AInput(loc: Loc) extends AExpr with AAtomicExpr {
  override def toString: String = s"${toCustomString(PartialFunction.empty)}:$loc"

  def toCustomString(printer: PartialFunction[AstNode, String]): String =
    printer.applyOrElse(this, { _: AInput =>
      "input"
    })
}

case class AMalloc(loc: Loc) extends AExpr with AAtomicExpr {
  override def toString: String = s"malloc:$loc"

  def toCustomString(printer: PartialFunction[AstNode, String]): String =
    printer.applyOrElse(this, { _: AMalloc =>
      "malloc"
    })
}

case class ANull(loc: Loc) extends AExpr with AAtomicExpr {
  override def toString: String = s"${toCustomString(PartialFunction.empty)}:$loc"

  def toCustomString(printer: PartialFunction[AstNode, String]): String =
    printer.applyOrElse(this, { _: ANull =>
      "null"
    })
}

//////////////// Statements //////////////////////////

sealed trait AStmt extends AstNode

/**
  * A statement in the body of a nested block (cannot be a declaration or a return).
  */
sealed trait AStmtInNestedBlock extends AStmt

case class AAssignStmt(left: AstNode.Assignable, right: AExpr, loc: Loc) extends AStmtInNestedBlock {
  override def toString: String = s"${toCustomString(PartialFunction.empty)}:$loc"

  def toCustomString(printer: PartialFunction[AstNode, String]): String =
    printer.applyOrElse(this, { _: AAssignStmt =>
      s"${left.fold(_.toCustomString(printer), _.toCustomString(printer))} = ${right.toCustomString(printer)};"
    })
}

sealed trait ABlock extends AStmt {

  /**
    * All the statements in the block, in order.
    */
  def body: List[AStmt]

  override def toString: String = s"${toCustomString(PartialFunction.empty)}:$loc"

  def toCustomString(printer: PartialFunction[AstNode, String]): String =
    printer.applyOrElse(this, { _: ABlock =>
      {
        s"{\n${body.map(_.toCustomString(printer)).mkString("\n")}\n}"
      }
    })
}

case class ANestedBlockStmt(body: List[AStmtInNestedBlock], loc: Loc) extends ABlock with AStmtInNestedBlock

case class AFunBlockStmt(declarations: List[AVarStmt], others: List[AStmtInNestedBlock], ret: AReturnStmt, loc: Loc) extends ABlock {

  /**
    * The contents of the block, not partitioned into declarations, others and return
    */
  val body: List[AStmt] = declarations ++ (others :+ ret)
}

case class AIfStmt(guard: AExpr, ifBranch: AStmtInNestedBlock, elseBranch: Option[AStmtInNestedBlock], loc: Loc) extends AStmtInNestedBlock {
  override def toString: String = s"${toCustomString(PartialFunction.empty)}:$loc"

  def toCustomString(printer: PartialFunction[AstNode, String]): String =
    printer.applyOrElse(this, { _: AIfStmt =>
      {
        val elseb = elseBranch.map(x => "else " + x.toCustomString(printer)).getOrElse("")
        s"if(${guard.toCustomString(printer)}) ${ifBranch.toCustomString(printer)}  $elseb"
      }
    })
}

case class AOutputStmt(value: AExpr, loc: Loc) extends AStmtInNestedBlock {
  override def toString: String = s"${toCustomString(PartialFunction.empty)}:$loc"

  def toCustomString(printer: PartialFunction[AstNode, String]): String = {
    printer.applyOrElse(this, { _: AOutputStmt =>
      s"output ${value.toCustomString(printer)};"
    })
  }
}

case class AReturnStmt(value: AExpr, loc: Loc) extends AStmt {
  override def toString: String = s"${toCustomString(PartialFunction.empty)}:$loc"

  def toCustomString(printer: PartialFunction[AstNode, String]): String =
    printer.applyOrElse(this, { _: AReturnStmt =>
      s"return ${value.toCustomString(printer)};"
    })
}

case class AErrorStmt(value: AExpr, loc: Loc) extends AStmtInNestedBlock {
  override def toString: String = s"${toCustomString(PartialFunction.empty)}:$loc"

  def toCustomString(printer: PartialFunction[AstNode, String]): String =
    printer.applyOrElse(this, { _: AErrorStmt =>
      s"error ${value.toCustomString(printer)};"
    })
}

case class AVarStmt(declIds: List[AIdentifierDeclaration], loc: Loc) extends AStmt {
  override def toString: String = s"${toCustomString(PartialFunction.empty)}:$loc"

  def toCustomString(printer: PartialFunction[AstNode, String]): String =
    printer.applyOrElse(this, { _: AVarStmt =>
      s"var ${declIds.map(_.toCustomString(printer)).mkString(",")};"
    })
}

case class AWhileStmt(guard: AExpr, innerBlock: AStmtInNestedBlock, loc: Loc) extends AStmtInNestedBlock {
  override def toString: String = s"${toCustomString(PartialFunction.empty)}:$loc"

  def toCustomString(printer: PartialFunction[AstNode, String]): String =
    printer.applyOrElse(this, { _: AWhileStmt =>
      s"while(${guard.toCustomString(printer)}) ${innerBlock.toCustomString(printer)}"
    })
}

//////////////// Program and function ///////////////

case class AProgram(funs: List[AFunDeclaration], loc: Loc) extends AstNode {

  def mainFunction: AFunDeclaration = {
    val main = funs.find(decl => decl.name == "main")
    if (main.isDefined) main.get
    else throw new RuntimeException(s"Missing main function, declared functions are $funs")
  }

  override def toString: String = s"${toCustomString(PartialFunction.empty)}:$loc"

  def toCustomString(printer: PartialFunction[AstNode, String]): String =
    printer.applyOrElse(this, { _: AProgram =>
      s"${funs.map(_.toCustomString(printer)).mkString("\n\n")}"
    })
}

case class AFunDeclaration(name: String, args: List[AIdentifierDeclaration], stmts: AFunBlockStmt, loc: Loc) extends ADeclaration {
  override def toString: String = s"$name (${args.mkString(",")}){...}"

  def toCustomString(printer: PartialFunction[AstNode, String]): String =
    printer.applyOrElse(this, { _: AFunDeclaration =>
      s"$name (${args.map(_.toCustomString(printer)).mkString(",")})\n${stmts.toCustomString(printer)}"
    })
}
