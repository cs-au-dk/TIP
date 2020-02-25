package tip.analysis

import tip.ast.{AAssignStmt, AIdentifier, AProgram, AstNode, DepthFirstAstVisitor, _}
import tip.solvers.CubicSolver
import tip.util.Log
import tip.ast.AstNodeData.{AstNodeWithDeclaration, DeclarationData}

import scala.language.implicitConversions

class ControlFlowAnalysis(program: AProgram)(implicit declData: DeclarationData)
    extends DepthFirstAstVisitor[Null]
    with Analysis[Map[AstNode, Set[AFunDeclaration]]] {

  val log = Log.logger[this.type]()

  case class Decl(fun: AFunDeclaration) {
    override def toString = s"${fun.name}:${fun.loc}"
  }

  case class AstVariable(n: AstNode) {
    override def toString = n match {
      case fun: AFunDeclaration => s"${fun.name}:${fun.loc}"
      case _ => n.toString
    }
  }

  private val solver = new CubicSolver[AstVariable, Decl]

  val allFunctions: Set[AFunDeclaration] = program.funs.toSet

  NoPointers.assertContainsProgram(program)
  NoRecords.assertContainsProgram(program)

  /**
    * @inheritdoc
    */
  def analyze(): Map[AstNode, Set[AFunDeclaration]] = {
    visit(program, null)
    val sol = solver.getSolution
    log.info(s"Solution is:\n${sol.map { case (k, v) => s"  [[$k]] = {${v.mkString(",")}}" }.mkString("\n")}")
    sol.map(vardecl => vardecl._1.n -> vardecl._2.map(_.fun))
  }

  /**
    * Generates the constraints for the given sub-AST.
    * @param node the node for which it generates the constraints
    * @param arg unused for this visitor
    */
  def visit(node: AstNode, arg: Null) = {

    /**
      * Get the declaration if the supplied AstNode is an identifier,
      * which might be a variable declaration or a function declaration.
      * It returns the node itself, otherwise.
      */
    def decl(n: AstNode): AstNode = n match {
      case id: AIdentifier => id.declaration
      case _ => n
    }

    implicit def toVar(n: AstNode): AstVariable = AstVariable(n)

    node match {
      case fun: AFunDeclaration => ??? //<--- Complete here
      case AAssignStmt(id: AIdentifier, e, _) => ??? //<--- Complete here
      case ACallFuncExpr(targetFun, args, _) => ??? //<--- Complete here
      case _ =>
    }
    visitChildren(node, null)
  }
}
