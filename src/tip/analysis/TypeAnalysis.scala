package tip.analysis

import tip.ast._
import tip.solvers._
import tip.types._
import tip.ast.AstNodeData._
import tip.util.Log

/**
  * Unification-based type analysis.
  * The analysis associates a [[tip.types.TipType]] with each variable declaration and expression node in the AST.
  * It is implemented using [[tip.solvers.UnionFindSolver]].
  */
class TypeAnalysis(program: AProgram)(implicit declData: DeclarationData) extends DepthFirstAstVisitor[Null] with Analysis[TypeData] {

  val log = Log.logger[this.type](Log.Level.Debug)

  val solver = new UnionFindSolver[TipType]

  /**
    * @inheritdoc
    */
  def analyze(): TypeData = {

    // generate the constraints by traversing the AST and solve them on-the-fly
    visit(program, null)

    var ret: TypeData = Map()

    // close the terms and create the TypeData
    new DepthFirstAstVisitor[Null] {
      val sol = solver.solution()
      visit(program, null)

      // extract the type for each identifier declaration and each non-identifier expression
      override def visit(node: AstNode, arg: Null): Unit = {
        node match {
          case _: AIdentifier =>
          case _: ADeclaration | _: AExpr =>
            ret += node -> Some(TipTypeOps.close(TipVar(node), sol).asInstanceOf[TipType])
          case _ =>
        }
        visitChildren(node, null)
      }
    }

    log.info(s"Inferred types are:\n${ret.map { case (k, v) => s"$k: ${v.get}" }.mkString("\n")}")
    ret
  }

  /**
    * Generates the constraints for the given sub-AST.
    * @param node the node for which it generates the constraints
    * @param arg unused for this visitor
    */
  override def visit(node: AstNode, arg: Null): Unit = {
    log.debug(s"Visiting ${node.getClass.getSimpleName} at ${node.loc}")
    node match {
      case _: ANumber => ??? // <--- Complete here
      case _: AInput => ??? // <--- Complete here
      case iff: AIfStmt => ??? // <--- Complete here
      case out: AOutputStmt => ??? // <--- Complete here
      case whl: AWhileStmt => ??? // <--- Complete here
      case ass: AAssignStmt => ??? // <--- Complete here
      case bin: ABinaryOp =>
        bin.operator match {
          case Eqq => ??? // <--- Complete here
          case _ => ??? // <--- Complete here
        }
      case un: AUnaryOp[_] =>
        un.operator match {
          case RefOp => ??? // <--- Complete here
          case DerefOp => ??? // <--- Complete here
        }
      case _: AMalloc => ??? // <--- Complete here
      case _: ANull => ??? // <--- Complete here
      case fun: AFunDeclaration => ??? // <--- Complete here
      case call: ACallFuncExpr => ??? // <--- Complete here
      case _: AReturnStmt => ??? // <--- Complete here
      case _ =>
    }
    visitChildren(node, null)
  }

  private def unify(t1: Term[TipType], t2: Term[TipType]): Unit = {
    log.debug(s"Generating constraint $t1 = $t2")
    solver.unify(t1, t2)
  }
}
