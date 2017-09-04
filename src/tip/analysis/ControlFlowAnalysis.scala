package tip.analysis

import tip.ast.{AAssignStmt, AIdentifier, AProgram, AstNode, DepthFirstAstVisitor, NormalizedForPointsToAnalysis, _}
import tip.solvers.CubicSolver
import tip.util.Log
import tip.ast.AstNodeData.{AstNodeWithDeclaration, DeclarationData}

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

  val allFunctions = program.funs.toSet

  NormalizedForPointsToAnalysis.assertContainsProgram(program)

  /**
    * @inheritdoc
    */
  def analyze() = {
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
      case AAssignStmt(id1: AIdentifier, e: AIdentifier, _) => ??? //<--- Complete here
      case ACallFuncExpr(id2: AIdentifier, args, _) =>
        id2.declaration match {
          case fun: AFunDeclaration => // Simple call, using function name directly
            // Add the constraints concerning parameters
            fun.args.zip(args).foreach {
              case (formalParam, actualParam) =>
                actualParam match {
                  case actualParamId: AIdentifier =>
                    solver.addSubsetConstraint(AstVariable(decl(actualParamId)), AstVariable(formalParam))
                  case _: AAtomicExpr =>
                  case _: AExpr => NormalizedForPointsToAnalysis.LanguageRestrictionViolation(s"Unexpected expression as parameter: $node")
                }
            }
            // Add the constraints concerning the return
            val ret = fun.stmts.ret
            ret.value match {
              case retId: AIdentifier =>
                solver.addSubsetConstraint(AstVariable(decl(retId)), AstVariable(node))
              case _: AExpr =>
            }
          case _: AIdentifierDeclaration => // Computed call, using function pointer
            ??? //<--- Complete here
        }
      case _ =>
    }
    visitChildren(node, null)
  }
}
