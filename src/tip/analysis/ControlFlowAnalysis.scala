package tip.analysis

import tip.ast.{AAssignStmt, AIdentifier, AProgram, AstNode, DepthFirstAstVisitor, NormalizedForPointsToAnalysis, _}
import tip.solvers.CubicSolver
import tip.util.Log
import tip.ast.AstNodeData._

class ControlFlowAnalysis(program: AProgram)(implicit declData: DeclarationData)
    extends DepthFirstAstVisitor[Null]
    with Analysis[Map[AstNode, Set[AFunDeclaration]]] {

  val log = Log.logger[this.type](Log.Level.Info)

  case class Decl(fun: AFunDeclaration) {
    override def toString = s"Decl(${fun.name}:${fun.loc})"
  }

  case class AstVariable(n: AstNode) {
    override def toString = n match {
      case fun: AFunDeclaration => s"Var(${fun.name}:${fun.loc})"
      case _ => s"Var($n:${n.loc})"
    }
  }

  private val solver = new CubicSolver[AstVariable, Decl]

  val allFunctions = program.funs.toSet

  NormalizedForPointsToAnalysis.assertContainsProgram(program)

  /**
    * @inheritdoc
    */
  def analyze() = {
    visit(program, null)
    log.info(s"Solution is:\n$solver")
    solver.getSolution.map(vardecl => vardecl._1.n -> vardecl._2.map(_.fun))
  }

  /**
    * Generates the constraints for the given sub-AST.
    * @param node the node for which it generates the constraints
    * @param arg unused for this visitor
    */
  override def visit(node: AstNode, arg: Null) {

    /**
      * Get the declaration if the supplied AstNode is an identifier,
      * which might be a variable declaration or a function declaration.
      * It returns the node itself, otherwise.
      */
    def decl(n: AstNode): AstNode = n match {
      case id: AIdentifier => id.declaration
      case _ => n
    }

    node match {
      case fun: AFunDeclaration => ??? //<--- Complete here
      case AAssignStmt(Left(id1), e, _) => ??? //<--- Complete here
      case ACallFuncExpr(id2: AIdentifier, args, _) =>
        id2.declaration match {
          case fun: AFunDeclaration => // Simple call, using function name directly
            ??? //<--- Complete here
          case _: AIdentifier => // Computed call, using function pointer
            ??? //<--- Complete here
          case _ => ???
        }
      case _ =>
    }
    visitChildren(node, null)
  }
}
