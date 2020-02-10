package tip.analysis

import tip.ast._
import tip.solvers._
import tip.types._
import tip.ast.AstNodeData._
import tip.util.Log
import AstOps._

/**
  * Unification-based type analysis.
  * The analysis associates a [[tip.types.TipType]] with each variable declaration and expression node in the AST.
  * It is implemented using [[tip.solvers.UnionFindSolver]].
  *
  * To novice Scala programmers:
  * The parameter `declData` is declared as "implicit", which means that invocations of `TypeAnalysis` obtain its value implicitly:
  * The call to `new TypeAnalysis` in Tip.scala does not explicitly provide this parameter, but it is in scope of
  * `implicit val declData: TypeData = new DeclarationAnalysis(programNode).analyze()`.
  * The TIP implementation uses implicit parameters many places to provide easy access to the declaration information produced
  * by `DeclarationAnalysis` and the type information produced by `TypeAnalysis`.
  * For more information about implicit parameters in Scala, see [[https://docs.scala-lang.org/tour/implicit-parameters.html]].
  */
class TypeAnalysis(program: AProgram)(implicit declData: DeclarationData) extends DepthFirstAstVisitor[Null] with Analysis[TypeData] {

  val log = Log.logger[this.type]()

  val solver = new UnionFindSolver[TipType]

  implicit val allFieldNames: List[String] = program.appearingFields.toList.sorted

  /**
    * @inheritdoc
    */
  def analyze(): TypeData = {

    // generate the constraints by traversing the AST and solve them on-the-fly
    visit(program, null)

    var ret: TypeData = Map()

    // close the terms and create the TypeData
    new DepthFirstAstVisitor[Null] {
      val sol: Map[Var[TipType], Term[TipType]] = solver.solution()
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

    log.info(s"Inferred types are:\n${ret.map { case (k, v) => s"  [[$k]] = ${v.get}" }.mkString("\n")}")
    ret
  }

  /**
    * Generates the constraints for the given sub-AST.
    * @param node the node for which it generates the constraints
    * @param arg unused for this visitor
    */
  def visit(node: AstNode, arg: Null): Unit = {
    log.verb(s"Visiting ${node.getClass.getSimpleName} at ${node.loc}")
    node match {
      case program: AProgram => unify(program.mainFunction.stmts.ret, TipInt())
                                program.mainFunction.params.foreach(unify(_, TipInt()))
      case _: ANumber => unify(node, TipInt()) 
      case _: AInput => unify(node, TipInt()) 
      case iff: AIfStmt => unify(iff.guard, TipInt()) 
      case out: AOutputStmt => unify(out.value, TipInt()) 
      case whl: AWhileStmt => unify(whl.guard, TipInt()) 
      case ass: AAssignStmt => unify(ass.left, ass.right) 
      case bin: ABinaryOp =>
        bin.operator match {
          case Eqq =>
            unify(bin.left, bin.right)
            unify(bin, TipInt) 
          case _ =>
            unify(bin.left, TipInt()) 
            unify(bin.right, TipInt()) 
            unify(bin, TipInt()) 
        }
      case un: AUnaryOp =>
        un.operator match {
          case RefOp => unify(un, TipRef(un.target)) 
          case DerefOp => unify(un.target, TipRef(un)) 
        }
      case alloc: AAlloc => unify(alloc, TipRef(TipAlpha())) 
      case _: ANull => unify(node, TipRef(TipAlpha())) 
      case fun: AFunDeclaration => unify(fun, TipFunction(
        fun.params.map(TipType.ast2typevar(_)),
        fun.stmts.ret.value)) 
      case call: ACallFuncExpr => unify(call.targetFun, TipFunction(
        call.args.map(TipType.ast2typevar(_)),
        call
      )) 
      case _: AReturnStmt =>
      case rec: ARecord =>
        val fieldmap = rec.fields.foldLeft(Map[String, Term[TipType]]()) { (a, b) =>
          a + (b.field -> b.exp)
        }
        unify(rec, TipRecord(allFieldNames.map { f =>
          fieldmap.getOrElse(f, TipAlpha(rec, f))
        }))
      case ac: AAccess =>
        unify(ac.record, TipRecord(allFieldNames.map { f =>
          if (f == ac.field) TipVar(ac) else TipAlpha(ac, f)
        }))
      case _ =>
    }
    visitChildren(node, null)
  }

  private def unify(t1: Term[TipType], t2: Term[TipType]): Unit = {
    log.verb(s"Generating constraint $t1 = $t2")
    solver.unify(t1, t2)
  }
}
