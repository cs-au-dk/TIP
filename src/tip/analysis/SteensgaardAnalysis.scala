package tip.analysis

import tip.ast.DepthFirstAstVisitor
import tip.logging.Log
import tip.solvers._
import tip.ast._

class SteensgaardAnalysis(program: AProgram)
  extends DepthFirstAstVisitor[Null] {

  val log = Log.typeLogger[this.type](Log.Level.Info)

  val solver = new UnionFindSolver[StTerm]

  performAnalysis()

  /**
   * Generates the constraints for the given sub-AST.
   * @param node the node for which it generates the constraints
   * @param arg unused for this visitor
   */
  override def visit(node: AstNode, arg: Null): Unit = {

    PointerNormalised.checkAstLanguageRestriction(node)

    /**
     * Peek the declaration of the identifier, either the variable declaration
     * or the function declaration for global functions
     */
    def pdef(id:AIdentifier): AIdentifierDeclaration = id.meta.definition.get

    node match {
      case AAssignStmt(id1: AIdentifier, malloc: AMalloc, _) =>
        //<---- Complete here
      case AAssignStmt(id1: AIdentifier, AUnaryOp(r:RefOp, id2: AIdentifier, _ ), _) =>
        //<---- Complete here
      case AAssignStmt(id1: AIdentifier, id2: AIdentifier, _) =>
        //<---- Complete here
      case AAssignStmt(id1: AIdentifier, AUnaryOp(d:DerefOp, id2: AIdentifier, _ ), _) =>
        //<---- Complete here
      case AAssignStmt(AUnaryOp(d: DerefOp, id1: AIdentifier, _), id2: AIdentifier, _) =>
        //<---- Complete here
      case AAssignStmt(id: AIdentifier, atom: AstAtom, _) =>
      case ass: AAssignStmt => PointerNormalised.LanguageRestrictionViolation(ass)
      case _ =>
    }

    visitChildren(node, null)
  }

  private def performAnalysis(): Unit = {
    visit(program, null)
    log.info(s"Sets: \n${solver.unifications.values.toSet.map{s: Set[Term[StTerm]] => s"{ ${s.mkString(",")} }"}.mkString("; ")}")
    log.info(s"Solution: \n$this")
  }
  
  def result: Map[IdentifierVariable, Set[StTerm]] = {
    val solution = solver.solution
    val unifications = solver.unifications
    val vars = solution.keys.collect{case id:IdentifierVariable => id}
    val pointsto = vars.foldLeft(Map[IdentifierVariable, Set[StTerm]]()) {
      case(a: Map[IdentifierVariable, Set[StTerm]], v: IdentifierVariable) =>
        val typeofv = solution(v)
        val pointsto = typeofv match {
          case p: PointerRef =>
            unifications(p.of).collect({case id: IdentifierVariable => id:StTerm; case mal: MallocVariable => mal:StTerm})
          case _ => Set[StTerm]() // If this variable is here, then there are no chances that it has ever been unified with a Pointer
        }
        a + (v -> pointsto) 
    }
    pointsto
  }

  override def toString(): String = {
    val res = result
    res.map(p => s"${p._1} = { ${p._2.mkString(",")} }").mkString("\n")
  }
}

/**
 * Counter for producing fresh IDs.
 */
object Fresh {

  var n = 0

  def next = {
    n += 1
    n
  }
}

/**
 * Terms used in unification.
 */
sealed trait StTerm

/**
 * A term variable that represents a malloc in the program.
 */
case class MallocVariable(malloc: AMalloc) extends StTerm with Var[StTerm] {

  override def toString: String = s"malloc-${malloc.offset}"
}

/**
 * A term variable that represents an identifier in the program.
 */
case class IdentifierVariable(id: AIdentifierDeclaration) extends StTerm with Var[StTerm] {

  override def toString: String = s"$id:${id.offset}"
}

/**
 * A fresh term variable.
 */
case class FreshVariable(var id: Int = 0) extends StTerm with Var[StTerm] {

  id = Fresh.next

  override def toString: String = s"\u03B1$id"
}

/**
 * A constructor term that represents a pointer to another term.
 * The tag is used to distinguish between different pointers that point to the same term.
 */
case class PointerRef(of: Term[StTerm], tag: Any) extends StTerm with Cons[StTerm] {

  def fv: Set[Var[StTerm]] = of.fv

  def subst(v: Var[StTerm], t: Term[StTerm]): Term[StTerm] = PointerRef(of.subst(v, t), tag)

  override def toString: String = s"&$tag$of"

  override def arity: Int = 1

  override def args: Seq[Term[StTerm]] = Seq(of)
}
