package tip.types

import tip.ast._
import tip.solvers._
import tip.ast.AstNodeData._
import scala.language.implicitConversions

object Type {

  /**
    * Implicitly converts any AstNode to its type variable.
    * For identifiers the type variable is associated with the declaration;
    * for any other kind of AST node the type variable is associated with the node itself.
    *
    * To novice Scala programmers:
    * The keyword `implicit` in front of this function definition enables "implicit conversion" from `AstNode` to `Var[TipType]`.
    * This means that whenever Scala finds something of type `AstNode` but needs something of type `Var[TipType]`, this
    * function will be invoked implicitly to make the conversion (provided that the function is imported).
    * For more information about implicit conversions in Scala, see [[https://docs.scala-lang.org/tour/implicit-conversions.html]].
    */
  implicit def ast2typevar(node: AstNode)(implicit declData: DeclarationData): Var[Type] =
    node match {
      case id: AIdentifier => VarType(declData(id))
      case _ => VarType(node)
    }

  implicit def ast2typevar(nodes: List[AstNode])(implicit declData: DeclarationData): List[Var[Type]] =
    nodes.map(ast2typevar)
}

/**
  * Counter for producing fresh IDs.
  */
object Fresh {

  var n = 0

  def next(): Int = {
    n += 1
    n
  }
}

/**
  * A type for a TIP variable or expression.
  */
sealed trait Type

object TipTypeOps extends TermOps[Type] {

  def makeFreshVar(): Var[Type] = FreshVarType()

  def makeMu(v: Var[Type], t: Term[Type]): Mu[Type] = RecursiveType(v, t)
}

/**
  * Int type.
  */
case class IntType() extends Type with Cons[Type] {

  val args: List[Term[Type]] = List()

  def subst(v: Var[Type], t: Term[Type]): Term[Type] = this

  override def toString: String = "int"
}

/**
  * Function type.
  */
case class FunctionType(params: List[Term[Type]], ret: Term[Type]) extends Type with Cons[Type] {

  val args: List[Term[Type]] = ret :: params

  def subst(v: Var[Type], t: Term[Type]): Term[Type] =
    FunctionType(params.map { p =>
      p.subst(v, t)
    }, ret.subst(v, t))

  override def toString: String = s"(${params.mkString(",")}) -> $ret"
}

/**
  * Pointer type.
  */
case class PointerType(of: Term[Type]) extends Type with Cons[Type] {

  val args: List[Term[Type]] = List(of)

  def subst(v: Var[Type], t: Term[Type]): Term[Type] = PointerType(of.subst(v, t))

  override def toString: String = s"\u2B61$of"
}

/**
  * Record type.
  *
  * A record type is represented as a term with a sub-term for every field name in the entire program.
  * (This could be represented more concisely by using AbsentFieldType as a default.)
  */
case class RecordType(args: List[Term[Type]])(implicit allFieldNames: List[String]) extends Type with Cons[Type] {

  def subst(v: Var[Type], t: Term[Type]): Term[Type] =
    RecordType(args.map { p =>
      p.subst(v, t)
    })

  override def toString: String =
    s"{${allFieldNames
      .zip(args)
      .map(p => {
        s"${p._1}:${p._2}"
      })
      .mkString(",")}}"
}

case object AbsentFieldType extends Type with Cons[Type] {

  val args: List[Term[Type]] = List()

  def subst(v: Var[Type], t: Term[Type]): Term[Type] = this

  override def toString: String = "\u25C7"
}

/**
  * Type variable for a program variable or expression.
  */
case class VarType(node: AstNode) extends Type with Var[Type] {

  require(!node.isInstanceOf[AIdentifier], "Trying to construct type variable for identifier expression");

  override def toString: String = s"\u27E6$node\u27E7"
}

/**
  * Fresh type variable.
  */
case class FreshVarType(var id: Int = 0) extends Type with Var[Type] {

  id = Fresh.next()

  override def toString: String = s"x$id"
}

/**
  * Recursive type (only created when closing terms).
  */
case class RecursiveType(v: Var[Type], t: Term[Type]) extends Type with Mu[Type] {

  def subst(sv: Var[Type], to: Term[Type]): Term[Type] =
    if (sv == v) this else RecursiveType(v, t.subst(sv, to))
}
