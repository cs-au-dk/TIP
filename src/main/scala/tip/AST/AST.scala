package tip.newAST

import tip.solvers.Term
import tip.types.TipType

import scala.collection.immutable

/**
 * A class holding all the meta-informations about an AST node collected during
 * the various phases of the analysis
 *
 * @param definition the definition point point of the node (available for identifiers only)
 * @param theType the type of the node (available for expressions only)
 */
case class AstMetadata(
  var definition: Option[ASTNode] = None,
  var theType: Option[TipType] = None) {

  def typeStr(): String = {
    theType match {
      case Some(t) => t.toString
      case None => "NotInferred"
    }
  }

}

case class Loc(line: Int, col: Int) {
  override def toString: String = s"$line:$col"
}

sealed trait Operator

case class Plus() extends Operator {
  override def toString: String = "+"
}

case class Minus() extends Operator {
  override def toString: String = "-"
}

case class Times() extends Operator {
  override def toString: String = "*"
}

case class Divide() extends Operator {
  override def toString: String = "/"
}

case class Eqq() extends Operator {
  override def toString: String = "=="
}

case class GreatThan() extends Operator {
  override def toString: String = ">"
}

case class RefOp() extends Operator {
  override def toString: String = "&"
}

case class DerefOp() extends Operator {
  override def toString: String = "*"
}

sealed abstract class ASTNode {
  def offset: Loc

  def getId: String = s"${this.getClass.getSimpleName}:$offset"

  def toTypedString(): String = toString
}

sealed trait ASTAtom extends ASTNode

//////////////// Expressions //////////////////////////

sealed trait AExpr extends ASTNode {
  var meta: AstMetadata
}

case class ACallFuncExpr(targetFun: AExpr, args: immutable.Seq[AExpr], offset: Loc)(var meta: AstMetadata = AstMetadata()) extends AExpr {
  override def toString(): String = s"$targetFun(${args.mkString(",")})"
}

case class AIdentifier(value: String, offset: Loc)(var meta: AstMetadata = AstMetadata()) extends AExpr with ASTAtom {
  override def toString(): String = value

  override def toTypedString(): String = {
    if (this.meta.definition.map(n => n == this).getOrElse(false))
      s"$value: ${this.meta.typeStr()}"
    else
      toString()
  }
}

case class ABinaryOp(operator: Operator, left: AExpr, right: AExpr, offset: Loc)(var meta: AstMetadata = AstMetadata()) extends AExpr {
  override def toString(): String = left + " " + operator + " " + right
}

case class AUnaryOp(operator: Operator, target: AExpr, offset: Loc)(var meta: AstMetadata = AstMetadata()) extends AExpr {
  override def toString(): String = s"$operator$target"
}

case class ANumber(value: Int, offset: Loc)(var meta: AstMetadata = AstMetadata()) extends AExpr with ASTAtom {
  override def toString(): String = s"$value"
}

case class AInput(offset: Loc)(var meta: AstMetadata = AstMetadata()) extends AExpr with ASTAtom {
  override def toString(): String = "input"
}

case class AMalloc(offset: Loc)(var meta: AstMetadata = AstMetadata()) extends AExpr with ASTAtom {
  override def toString(): String = "malloc"
}

case class ANull(offset: Loc)(var meta: AstMetadata = AstMetadata()) extends AExpr with ASTAtom {
  override def toString(): String = "null"
}

//////////////// Statements //////////////////////////

sealed trait AStmt extends ASTNode

case class AAssignStmt(left: AExpr, right: AExpr, offset: Loc) extends AStmt {
  override def toString(): String = s"$left = $right;"

  override def toTypedString(): String = s"${left.toTypedString()} = ${right.toTypedString()};"
}

case class ABlockStmt(content: immutable.Seq[AStmt], offset: Loc) extends AStmt {
  override def toString(): String = s"{\n${content.mkString("\n")}\n}"

  override def toTypedString(): String = s"{\n${content.map(_.toTypedString()).mkString("\n")}\n}"
}

case class AIfStmt(guard: AExpr, ifBranch: AStmt, elseBranch: Option[AStmt], offset: Loc) extends AStmt {
  override def toString(): String = {
    val elseb = elseBranch.map("else " + _).getOrElse("")
    s"if($guard) $ifBranch  ${elseb}"
  }

  override def toTypedString(): String = {
    val elseb = elseBranch.map("else " + _.toTypedString()).getOrElse("")
    s"if(${guard.toTypedString()}) ${ifBranch.toTypedString()}  ${elseb}"
  }
}

case class AoutputStmt(value: AExpr, offset: Loc) extends AStmt {
  override def toString(): String = s"output $value;"
}

case class AReturnStmt(value: AExpr, offset: Loc) extends AStmt {
  override def toString(): String = s"return $value;"
}

case class AVarStmt(declIds: immutable.Seq[AIdentifier], offset: Loc) extends AStmt {
  override def toString(): String = s"var ${declIds.mkString(",")};"

  override def toTypedString(): String = {
    val typedVar = declIds.map(_.toTypedString()).mkString(",")
    s"var $typedVar;"
  }
}

case class AWhileStmt(guard: AExpr, innerBlock: AStmt, offset: Loc) extends AStmt {
  override def toString(): String = s"while(${guard.toTypedString()}) $innerBlock"

  override def toTypedString(): String = s"while(${guard.toTypedString()}) ${innerBlock.toTypedString()}"
}

//////////////// Program and function ///////////////

case class AProgram(fun: immutable.Seq[AFunDeclaration], offset: Loc) extends ASTNode {
  override def toString(): String = s"${fun.mkString("\n\n")}"

  override def toTypedString(): String = s"${fun.map(_.toTypedString).mkString("\n\n")}"
}

case class AFunDeclaration(name: AIdentifier, args: immutable.Seq[AIdentifier], stmts: ABlockStmt, offset: Loc)(var meta: AstMetadata = AstMetadata()) extends ASTNode {
  override def toString(): String = s"$name (${args.mkString(",")})\n$stmts"

  override def toTypedString(): String = s"${name} (${args.map(_.toTypedString()).mkString(",")}): ${this.meta.typeStr()}\n${stmts.toTypedString()}"
}
