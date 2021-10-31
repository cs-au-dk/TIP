package tip.analysis

import tip.ast.{ADeclaration, DepthFirstAstVisitor, _}
import tip.solvers._
import tip.util.Log
import tip.ast.AstNodeData.DeclarationData
import scala.language.implicitConversions

/**
  * Steensgaard-style pointer analysis.
  * The analysis associates an [[StTerm]] with each variable declaration and expression node in the AST.
  * It is implemented using [[tip.solvers.UnionFindSolver]].
  */
class SteensgaardAnalysis(program: AProgram)(implicit declData: DeclarationData) extends DepthFirstAstVisitor[Unit] with PointsToAnalysis {

  val log = Log.logger[this.type]()

  val solver = new UnionFindSolver[StTerm]

  NormalizedForPointsToAnalysis.assertContainsProgram(program)
  NoRecords.assertContainsProgram(program)

  /**
    * @inheritdoc
    */
  def analyze(): Unit =
    // generate the constraints by traversing the AST and solve them on-the-fly
    visit(program, ())

  /**
    * Generates the constraints for the given sub-AST.
    * @param node the node for which it generates the constraints
    * @param arg unused for this visitor
    */
  def visit(node: AstNode, arg: Unit): Unit = {

    implicit def identifierToTerm(id: AIdentifier): Term[StTerm] = IdentifierVariable(id)
    implicit def allocToTerm(alloc: AAlloc): Term[StTerm] = AllocVariable(alloc)

    log.verb(s"Visiting ${node.getClass.getSimpleName} at ${node.loc}")
    node match {
      case AAssignStmt(id1: AIdentifier, alloc: AAlloc, _) => ??? //<--- Complete here
      case AAssignStmt(id1: AIdentifier, AVarRef(id2: AIdentifier, _), _) => ??? //<--- Complete here
      case AAssignStmt(id1: AIdentifier, id2: AIdentifier, _) => ??? //<--- Complete here
      case AAssignStmt(id1: AIdentifier, AUnaryOp(DerefOp, id2: AIdentifier, _), _) => ??? //<--- Complete here
      case AAssignStmt(ADerefWrite(id1: AIdentifier, _), id2: AIdentifier, _) => ??? //<--- Complete here
      case _ => // ignore other kinds of nodes
    }
    visitChildren(node, ())
  }

  private def unify(t1: Term[StTerm], t2: Term[StTerm]): Unit = {
    log.verb(s"Generating constraint $t1 = $t2")
    solver.unify(t1, t2) // note that unification cannot fail, because there is only one kind of term constructor and no constants
  }

  /**
    * @inheritdoc
    */
  def pointsTo(): Map[ADeclaration, Set[AstNode]] = {
    val solution = solver.solution()
    val unifications = solver.unifications()
    log.info(s"Solution: \n${solution.mkString(",\n")}")
    log.info(s"Sets: \n${unifications.values.map { s =>
      s"{ ${s.mkString(",")} }"
    }.mkString(", ")}")

    val vars = solution.keys.collect { case id: IdentifierVariable => id }
    val pointsto = vars.foldLeft(Map[ADeclaration, Set[AstNode]]()) {
      case (a, v: IdentifierVariable) =>
        val pt = unifications(solution(v))
          .collect({ case PointerRef(IdentifierVariable(id)) => id; case PointerRef(AllocVariable(alloc)) => alloc })
          .toSet
        a + (v.id -> pt)
    }
    log.info(s"Points-to:\n${pointsto.map(p => s"${p._1} -> { ${p._2.mkString(",")} }").mkString("\n")}")
    pointsto
  }

  /**
    * @inheritdoc
    */
  def mayAlias(): (ADeclaration, ADeclaration) => Boolean = {
    val solution = solver.solution()
    (id1: ADeclaration, id2: ADeclaration) =>
      val sol1 = solution(IdentifierVariable(id1))
      val sol2 = solution(IdentifierVariable(id2))
      sol1 == sol2 && sol1.isInstanceOf[PointerRef] // same equivalence class, and it contains a reference
  }
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
  * Terms used in unification.
  */
sealed trait StTerm

/**
  * A term variable that represents an alloc in the program.
  */
case class AllocVariable(alloc: AAlloc) extends StTerm with Var[StTerm] {

  override def toString: String = s"\u27E6alloc{${alloc.loc}}\u27E7"
}

/**
  * A term variable that represents an identifier in the program.
  */
case class IdentifierVariable(id: ADeclaration) extends StTerm with Var[StTerm] {

  override def toString: String = s"\u27E6$id\u27E7"
}

/**
  * A fresh term variable.
  */
case class FreshVariable(var id: Int = 0) extends StTerm with Var[StTerm] {

  id = Fresh.next()

  override def toString: String = s"x$id"
}

/**
  * A constructor term that represents a pointer to another term.
  */
case class PointerRef(of: Term[StTerm]) extends StTerm with Cons[StTerm] {

  val args: List[Term[StTerm]] = List(of)

  def subst(v: Var[StTerm], t: Term[StTerm]): Term[StTerm] = PointerRef(of.subst(v, t))

  override def toString: String = s"\u2B61$of"
}
