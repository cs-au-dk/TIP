package tip.analysis

import tip.ast._
import tip.solvers._
import tip.types._
import tip.ast.AstNodeData._
import tip.util.{Log, TipProgramException}
import AstOps._

import scala.collection.mutable

/**
  * Unification-based type analysis.
  * The analysis associates a [[tip.types.Type]] with each variable declaration and expression node in the AST.
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
class TypeAnalysis(program: AProgram)(implicit declData: DeclarationData) extends DepthFirstAstVisitor[Unit] with Analysis[TypeData] {

  val log = Log.logger[this.type]()

  val solver = new UnionFindSolver[Type]

  implicit val allFieldNames: List[String] = program.appearingFields.toList.sorted

  /**
    * @inheritdoc
    */
  def analyze(): TypeData = {

    // generate the constraints by traversing the AST and solve them on-the-fly
    try {
      visit(program, ())
    } catch {
      case e: UnificationFailure =>
        throw new TipProgramException(s"Type error: ${e.getMessage}")
    }

    // check for accesses to absent record fields
    new DepthFirstAstVisitor[Unit] {
      visit(program, ())

      override def visit(node: AstNode, arg: Unit): Unit = {
        node match {
          case ac: AFieldAccess =>
            if (solver.find(node).isInstanceOf[AbsentFieldType.type])
              throw new TipProgramException(s"Type error: Reading from absent field ${ac.field} ${ac.loc.toStringLong}")
          case as: AAssignStmt =>
            as.left match {
              case dfw: ADirectFieldWrite =>
                if (solver.find(as.right).isInstanceOf[AbsentFieldType.type])
                  throw new TipProgramException(s"Type error: Writing to absent field ${dfw.field} ${dfw.loc.toStringLong}")
              case ifw: AIndirectFieldWrite =>
                if (solver.find(as.right).isInstanceOf[AbsentFieldType.type])
                  throw new TipProgramException(s"Type error: Writing to absent field ${ifw.field} ${ifw.loc.toStringLong}")
              case _ =>
            }
          case _ =>
        }
        visitChildren(node, ())
      }
    }

    var ret: TypeData = Map()

    // close the terms and create the TypeData
    new DepthFirstAstVisitor[Unit] {
      val sol: Map[Var[Type], Term[Type]] = solver.solution()
      log.info(s"Solution (not yet closed):\n${sol.map { case (k, v) => s"  \u27E6$k\u27E7 = $v" }.mkString("\n")}")
      val freshvars: mutable.Map[Var[Type], Var[Type]] = mutable.Map()
      visit(program, ())

      // extract the type for each identifier declaration and each non-identifier expression
      override def visit(node: AstNode, arg: Unit): Unit = {
        node match {
          case _: AIdentifier =>
          case _: ADeclaration | _: AExpr =>
            ret += node -> Some(TipTypeOps.close(VarType(node), sol, freshvars).asInstanceOf[Type])
          case _ =>
        }
        visitChildren(node, ())
      }
    }

    log.info(s"Inferred types:\n${ret.map { case (k, v) => s"  \u27E6$k\u27E7 = ${v.get}" }.mkString("\n")}")
    ret
  }

  /**
    * Generates the constraints for the given sub-AST.
    * @param node the node for which it generates the constraints
    * @param arg unused for this visitor
    */
  def visit(node: AstNode, arg: Unit): Unit = {
    log.verb(s"Visiting ${node.getClass.getSimpleName} at ${node.loc}")
    node match {
      case program: AProgram => ??? // <--- Complete here
      case _: ANumber => ??? // <--- Complete here
      case _: AInput => ??? // <--- Complete here
      case is: AIfStmt => ??? // <--- Complete here
      case os: AOutputStmt => ??? // <--- Complete here
      case ws: AWhileStmt => ??? // <--- Complete here
      case as: AAssignStmt =>
        as.left match {
          case id: AIdentifier => ??? // <--- Complete here
          case dw: ADerefWrite => ??? // <--- Complete here
          case dfw: ADirectFieldWrite => ??? // <--- Complete here
          case ifw: AIndirectFieldWrite => ??? // <--- Complete here
        }
      case bin: ABinaryOp =>
        bin.operator match {
          case Eqq => ??? // <--- Complete here
          case _ => ??? // <--- Complete here
        }
      case un: AUnaryOp =>
        un.operator match {
          case DerefOp => ??? // <--- Complete here
        }
      case alloc: AAlloc => ??? // <--- Complete here
      case ref: AVarRef => ??? // <--- Complete here
      case _: ANull => ??? // <--- Complete here
      case fun: AFunDeclaration => ??? // <--- Complete here
      case call: ACallFuncExpr => ??? // <--- Complete here
      case _: AReturnStmt =>
      case rec: ARecord =>
        val fieldmap = rec.fields.foldLeft(Map[String, Term[Type]]()) { (a, b) =>
          a + (b.field -> b.exp)
        }
        unify(rec, RecordType(allFieldNames.map { f =>
          fieldmap.getOrElse(f, AbsentFieldType)
        }))
      case ac: AFieldAccess =>
        unify(ac.record, RecordType(allFieldNames.map { f =>
          if (f == ac.field) VarType(ac) else FreshVarType()
        }))
      case _ =>
    }
    visitChildren(node, ())
  }

  private def unify(t1: Term[Type], t2: Term[Type]): Unit = {
    log.verb(s"Generating constraint $t1 = $t2")
    solver.unify(t1, t2)
  }
}
